package fit.lang.plugin.json.cloud;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.web.websocket.WebSocketClient;
import fit.lang.plugin.json.web.websocket.WebSocketClientHandler;
import io.netty.bootstrap.Bootstrap;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioSocketChannel;
import io.netty.handler.codec.http.DefaultHttpHeaders;
import io.netty.handler.codec.http.HttpClientCodec;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.codec.http.websocketx.WebSocketClientHandshakerFactory;
import io.netty.handler.codec.http.websocketx.WebSocketVersion;
import io.netty.handler.codec.http.websocketx.extensions.compression.WebSocketClientCompressionHandler;
import io.netty.handler.ssl.SslContext;
import io.netty.handler.ssl.SslContextBuilder;
import io.netty.handler.ssl.util.InsecureTrustManagerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonText;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.parseNodeUrl;
import static fit.lang.plugin.json.ExpressUtil.eval;

/**
 *
 */
public class CloudClientJsonExecuteNode extends JsonExecuteNode {

    WebSocketClient webSocketClient;

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        if (webSocketClient == null) {
            String url = parseNodeUrl(input.getInputParamAndContextParam(), nodeJsonDefine, "cloudServer");
            try {
                webSocketClient = new WebSocketClient(url);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }

        webSocketClient.send(input.getData().toJSONString());

        for (int i = 0; i < 1000; i++) {
            if (!WebSocketClientHandler.resultList.isEmpty()) {
                break;
            }
            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }

        String data = WebSocketClientHandler.resultList.get(0);
        WebSocketClientHandler.resultList.remove(0);

        JSONObject result = new JSONObject();
        result.put("message", "ok");
        if (isJsonText(data)) {
            result.put("data", JSONObject.parse(data));
        } else {
            result.put("data", data);
        }

        if (data == null) {
            result.put("message", "timeout!");
        }
        output.setData(result);
    }

}