package fit.lang.plugin.json.web;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.web.websocket.WebSocketServerHandler;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.*;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.codec.http.HttpServerCodec;
import io.netty.handler.stream.ChunkedWriteHandler;

import static fit.lang.plugin.json.tool.ServerJsonExecuteNode.buildServerPort;

/**
 * 执行节点
 * server:  <a href="https://blog.csdn.net/cold___play/article/details/131994054">server</a>
 * client : <a href="https://zhuanlan.zhihu.com/p/418968903">client</a>
 */
public class WebSocketServerJsonExecuteNode extends JsonExecuteNode {

    public static final int DEFAULT_SERVER_PORT = 10000;

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        Integer port = buildServerPort(input.getData(), nodeJsonDefine, DEFAULT_SERVER_PORT);

        JSONObject result = new JSONObject();

        try {
            run(port);
        } catch (Exception e) {
            //TODO
            throw new RuntimeException(e);
        }

        result.put("port", port);
        result.put("message", "start web socket at port: " + port);

        output.setData(result);
    }

    public void run(int port) throws Exception {
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
                            ChannelPipeline pipeline = ch.pipeline();
                            //HttpServerCodec，将请求和应答消息编码或者解码为HTTP消息
                            pipeline.addLast("http-codec", new HttpServerCodec());
                            //HttpObjectAggregator，将HTTP消息的多个部分组合成一条完整的HTTP消息
                            pipeline.addLast("aggregator", new HttpObjectAggregator(65535));
                            //ChunkedWriteHandler，用来向客户端发送HTML5文件，主要用于支持浏览器和服务端进行WebSocket通信
                            pipeline.addLast("http-chunked", new ChunkedWriteHandler());
                            //自定义处理协议内容
                            pipeline.addLast("handler", new WebSocketServerHandler());
                        }
                    });
            Channel ch = server.bind(port).sync().channel();
            System.out.println("Web socket server started at port " + port + ".");
            System.out.println("Open your browser and navigate to http://localhost:" + port + "/");
            ch.closeFuture().sync();
        } finally {
            boosGroup.shutdownGracefully();
            workerGroup.shutdownGracefully();
        }
    }
}
