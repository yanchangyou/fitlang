package fit.lang.plugin.json.cloud;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.lang.UUID;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.cloud.websocket.CloudWebSocketServerHandler;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.*;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.codec.http.HttpServerCodec;
import io.netty.handler.stream.ChunkedWriteHandler;

import java.util.*;

import static fit.lang.plugin.json.web.ServerJsonExecuteNode.buildServerPort;

/**
 *
 */
public class CloudServerJsonExecuteNode extends JsonExecuteNode {

    public static final int DEFAULT_SERVER_PORT = 20000;

    static Map<Integer, EventLoopGroup[]> eventLoopGroupMap = new HashMap<>();
    static Map<Integer, Channel> channelMap = new HashMap<>();


    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        Integer port = buildServerPort(input.getData(), nodeJsonDefine, DEFAULT_SERVER_PORT);

        new Thread() {
            @Override
            public void run() {
                try {
                    execute(port);
                } catch (Exception e) {
                    //TODO
                    throw new RuntimeException(e);
                }
            }
        }.start();

        JSONObject result = new JSONObject();
        result.put("port", port);
        result.put("message", "start web socket at port: " + port);

        output.setData(result);
    }

    public void execute(int port) throws Exception {
        EventLoopGroup boosGroup = new NioEventLoopGroup();
        EventLoopGroup workerGroup = new NioEventLoopGroup();
        try {
            ServerBootstrap server = new ServerBootstrap();
            server.group(boosGroup, workerGroup)
                    .channel(NioServerSocketChannel.class)
                    .option(ChannelOption.SO_BACKLOG, 1024)
                    .childHandler(new ChannelInitializer<SocketChannel>() {
                        @Override
                        protected void initChannel(SocketChannel ch) throws Exception {
//                            channel = ch;
                            ChannelPipeline pipeline = ch.pipeline();
                            //HttpServerCodec，将请求和应答消息编码或者解码为HTTP消息
                            pipeline.addLast("http-codec", new HttpServerCodec());
                            //HttpObjectAggregator，将HTTP消息的多个部分组合成一条完整的HTTP消息
                            pipeline.addLast("aggregator", new HttpObjectAggregator(65535));
                            //ChunkedWriteHandler，用来向客户端发送HTML5文件，主要用于支持浏览器和服务端进行WebSocket通信
                            pipeline.addLast("http-chunked", new ChunkedWriteHandler());
                            //自定义处理协议内容
                            pipeline.addLast("handler", new CloudWebSocketServerHandler());
//                            pipeline.addLast("default", globalGroup);
                        }
                    });
            Channel channel = server.bind(port).sync().channel();
            System.out.println("Web socket server started at port " + port + ".");
            System.out.println("Open your browser and navigate to http://localhost:" + port + "/");
//            eventLoopGroupMap.put(port, new EventLoopGroup[]{boosGroup, workerGroup});
            channelMap.put(port, channel);

            channel.closeFuture().sync();
            System.out.println("channel close!");
        } finally {
            boosGroup.shutdownGracefully();
            workerGroup.shutdownGracefully();
        }
    }

    /**
     * 停止所有的
     */
    public static void stopAll() {
        for (Map.Entry<Integer, Channel> entry : channelMap.entrySet()) {
            Channel channel = entry.getValue();
            channel.close();
            System.out.println("stop websocket: " + entry.getKey());
        }
        eventLoopGroupMap.clear();
    }

    static JSONObject clientMap = new JSONObject();

    public static String createSession(String clientId, JSONObject info, JSONObject attribute) {
        String sessionId = UUID.randomUUID().toString();
        JSONObject sessionInfo = new JSONObject();
        sessionInfo.put("sessionId", sessionId);
        sessionInfo.put("connectTime", DateUtil.now());
        sessionInfo.putAll(attribute);
        sessionInfo.put("info", info);
        clientMap.put(clientId, sessionInfo);
        return sessionId;
    }

    public static JSONObject getClientMap() {
        return clientMap;
    }

    public static List getSessions() {
        return Arrays.asList(clientMap.values().toArray());
    }

    public static JSONObject getSession(String clientId) {
        return clientMap.getJSONObject(clientId);
    }

}