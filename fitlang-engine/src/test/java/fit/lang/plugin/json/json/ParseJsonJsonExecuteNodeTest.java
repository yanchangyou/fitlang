package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ParseJsonJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'parseJson'," +
                "   'jsonField': 'json'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{ 'json': '{\"uni\":\"hello\"}'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("json"));
        Assert.assertTrue(outputJson.getJSONObject("json").containsKey("uni"));

        Assert.assertEquals("hello", outputJson.getJSONObject("json").get("uni"));

    }

    public void testExecuteArray() {
        String flow = "{" +//
                "   'uni': 'parseJson'," +
                "   'jsonField': 'json'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{ 'json': '[{\"uni\":\"hello\"}]'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("json"));
        Assert.assertEquals(1, outputJson.getJSONArray("json").size());

        Assert.assertEquals("hello", outputJson.getJSONArray("json").getJSONObject(0).get("uni"));

    }
}