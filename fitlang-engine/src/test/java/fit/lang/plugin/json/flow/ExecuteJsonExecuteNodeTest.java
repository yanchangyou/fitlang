package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ExecuteJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'execute'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'uni':'hello'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("message"));

        Assert.assertEquals("hello, world!", outputJson.getString("message"));

    }
}