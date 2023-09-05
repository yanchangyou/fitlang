package fit.lang.plugin.json.web.websocket;

import junit.framework.TestCase;

public class WebSocketClientTest extends TestCase {


    public void test() throws Exception {
        String url = "ws://127.0.0.1:10000/ws";
        WebSocketClient webSocketClient = new WebSocketClient(url);
    }
}