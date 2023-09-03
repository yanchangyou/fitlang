package fit.lang.plugin.json.web;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;

public class SendToWebSocketClientJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {

        startWebSocketServer();

        startWebSocketClient();

        sendToWebSocketMessage();

        Thread.sleep(1000);
    }

    public void startWebSocketServer() {
        String flow = "{" +//
                "   'uni': 'websocketServer'," +
                "}";
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
    }

    public void startWebSocketClient() {

        String flow = "{" +//
                "   'uni': 'websocketClient'," +
                "   'url': 'ws://127.0.0.1:10000/ws'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

    }


    public void sendToWebSocketMessage() {

        String flow = "{" +//
                "   'uni': 'sendToWebsocketClient'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'uni':'hello','message':'hello, websocket!'}", flow);

        System.out.println(output);

    }
}