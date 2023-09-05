package fit.lang.plugin.json.web;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;

public class WebSocketClientJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        String flow = "{" +//
                "   'uni': 'wsClient'," +
                "   'url': 'ws://127.0.0.1:10000/ws'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

    }

    public void testServerAndClient() throws InterruptedException {

        startWebSocketServer();
        Thread.sleep(1000);

        JSONObject message = new JSONObject();
        message.put("who", System.currentTimeMillis());
        message.put("uni", "hello");

        startWebSocketClient(message);

        Thread.sleep(1);

    }

    public void startWebSocketServer() {
        String flow = "{" +//
                "   'uni': 'wsServer'," +
                "}";
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
    }

    public void startWebSocketClient(JSONObject message) {

        String flow = "{" +//
                "   'uni': 'wsClient'," +
                "   'url': 'ws://127.0.0.1:10000/ws'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode(message.toJSONString(), flow);

        System.out.println(output);

    }


}