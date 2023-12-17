package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class HelloJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        HelloJsonExecuteNode helloExecuteNode = new HelloJsonExecuteNode();
        helloExecuteNode.setNodeDefine(JSONObject.parseObject("{'uni':'hello'}"));

        JSONObject output = ExecuteJsonNodeUtil.execute(JSON.parseObject("{'who':'world'}"), helloExecuteNode);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(output.containsKey("message"));

        Assert.assertEquals("hello, world!", output.getString("message"));

    }

    public void testExecute2() {
        String flow = "{" +//
                "   'uni': 'hello'," +
                "   'message': \"${'hello, ' + who + '!'}\"" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("message"));

        Assert.assertEquals("hello, world!", outputJson.getString("message"));

    }
}