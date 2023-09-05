package fit.lang.plugin.json.web.websocket;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.*;
import io.netty.channel.group.ChannelGroup;
import io.netty.channel.group.DefaultChannelGroup;
import io.netty.handler.codec.http.*;
import io.netty.handler.codec.http.websocketx.*;
import io.netty.util.CharsetUtil;
import io.netty.util.concurrent.GlobalEventExecutor;

import java.util.ArrayList;
import java.util.List;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonText;

/**
 *
 */
public class WebSocketServerHandler extends ChannelInboundHandlerAdapter {

    public static ChannelGroup CHANNEL_GROUP = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE);

    public static List<String> messageList = new ArrayList<>();

    public static List<String> resultList = new ArrayList<>();

    private WebSocketServerHandshaker handShaker;

    JSONObject serviceDefine;

    public WebSocketServerHandler(JSONObject serviceDefine) {
        this.serviceDefine = serviceDefine;
    }

    private static String getWebSocketLocation(FullHttpRequest req) {
        String location = req.headers().get("Host") + "/ws";
        return "ws://" + location;
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        //传统HTTP接入
        if (msg instanceof FullHttpRequest) {
            handleHttpRequest(ctx, (FullHttpRequest) msg);
        }
        //WebSocket接入
        else if (msg instanceof WebSocketFrame) {
            handleWebSocketFrame(ctx, (WebSocketFrame) msg);
        }
    }

    /**
     * 处理WebSocket接入
     *
     * @param ctx
     * @param frame
     */
    private void handleWebSocketFrame(ChannelHandlerContext ctx, WebSocketFrame frame) throws Exception {

        CHANNEL_GROUP.add(ctx.channel());

        //判断是否是关闭链路的指令
        if (frame instanceof CloseWebSocketFrame) {
            handShaker.close(ctx.channel(), (CloseWebSocketFrame) frame.retain());
            return;
        }
        //判断是否是Ping消息
        if (frame instanceof PingWebSocketFrame) {
            ctx.channel().write(new PongWebSocketFrame(frame.content().retain()));
            return;
        }

        //只支持文本消息，不支持二进制消息
        if (!(frame instanceof TextWebSocketFrame)) {
            throw new UnsupportedOperationException(String.format("%s frame types not supported", frame.getClass().getName()));
        }

        if (ctx == null || this.handShaker == null || ctx.isRemoved()) {
            throw new Exception("尚未握手成功，无法向客户端发送WebSocket消息");
        }

        //返回应答消息
        String request = ((TextWebSocketFrame) frame).text();
        System.out.println("receive massage: " + request);

        JSONObject result = new JSONObject();

        result.put("code", 0);

        String resultText = "";
        if (isJsonText(request)) {

            try {
                resultText = ExecuteJsonNodeUtil.executeCode(request);
                result.put("data", resultText);
            } catch (Exception e) {
                result.put("code", 500);
                result.put("message", e.getMessage());
                resultText = result.toJSONString();
            }
        } else {
            result.put("code", 400);
            result.put("message", "only support json!");
            resultText = result.toJSONString();
        }
        ctx.writeAndFlush(new TextWebSocketFrame(resultText));
    }

    /**
     * 处理HTTP接入
     *
     * @param ctx
     * @param req
     */
    private void handleHttpRequest(ChannelHandlerContext ctx, FullHttpRequest req) {
        //如果HTTP解码失败，返回HTTP异常
        //如果消息头中没有包含Upgrade字段或者它的值不是websocket，则返回Http 400响应
        if (!req.decoderResult().isSuccess() || (!"websocket".equals(req.headers().get("Upgrade")))) {
            sendHttpResponse(ctx, req, new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.BAD_REQUEST));
            return;
        }

        //构造握手响应返回，本机测试
        //构造握手工厂
        WebSocketServerHandshakerFactory wsFactory = new WebSocketServerHandshakerFactory(getWebSocketLocation(req), null, false);
        //创建握手处理类WebSocketServerHandshaker
        handShaker = wsFactory.newHandshaker(req);
        if (handShaker == null) {
            //不支持websocket协议
            WebSocketServerHandshakerFactory.sendUnsupportedVersionResponse(ctx.channel());
        } else {
            //构造我后响应消息返回给客户端
            ChannelFuture future = handShaker.handshake(ctx.channel(), req);
            if (future.isSuccess()) {
                //dosomething
//                future.channel().writeAndFlush(new TextWebSocketFrame("only support web socket!"));
            }
        }
    }

    /**
     * 发送HTTP响应
     *
     * @param ctx
     * @param req
     * @param res
     */
    private void sendHttpResponse(ChannelHandlerContext ctx, FullHttpRequest req, DefaultFullHttpResponse res) {
        //返回应答给客户端
        if (res.status().code() != 200) {
            ByteBuf buf = Unpooled.copiedBuffer(res.status().toString(), CharsetUtil.UTF_8);
            res.content().writeBytes(buf);
            buf.release();
            HttpUtil.setContentLength(res, res.content().readableBytes());
        }
        //发送应答消息
        ChannelFuture f = ctx.channel().writeAndFlush(res);
        //如果是非Keep-Alive，关闭连接
        if (!HttpUtil.isKeepAlive(req) || res.status().code() != 200) {
            f.addListener(ChannelFutureListener.CLOSE);
        }
    }

    @Override
    public void channelReadComplete(ChannelHandlerContext ctx) {

    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
        ctx.close();
    }
}