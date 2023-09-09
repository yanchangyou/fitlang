package fit.lang.plugin.json.cloud;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class CloudClientJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        startServer();

        startClient();

        System.out.println(CloudServerJsonExecuteNode.getSessionMap());

        CloudServerJsonExecuteNode.stopAll();

    }

    public void startServer() {

        String flow = "{'uni':'cloudServer','port':20000}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"port\":20000,\"message\":\"start web socket at port: 20000\"}", output);

    }

    private static void startClient() {
        String flow = "{'uni':'cloudClient','cloudServer':'ws://127.0.0.1:20000/'}";

        String output = ExecuteJsonNodeUtil.executeCode("{'name':'coder', 'sayHello':'hello, every one!'}", flow);

        System.out.println(output);

        Assert.assertTrue(JSONObject.parseObject(output).getJSONObject("meta").containsKey("sessionId"));

    }

}