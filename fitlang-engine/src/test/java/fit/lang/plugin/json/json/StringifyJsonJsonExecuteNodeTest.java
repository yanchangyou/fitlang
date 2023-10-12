package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class StringifyJsonJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'stringifyJson'," +
                "   'jsonField': 'json'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{ 'json': {'uni':'hello'}}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("json"));

        Assert.assertEquals("{\"uni\":\"hello\"}", outputJson.getString("json"));

    }

    public void testExecute2() {
        String flow = "{" +//
                "   'uni': 'stringifyJson'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{ 'json': {'uni':'hello'}}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertEquals("{\"json\":{\"uni\":\"hello\"}}", outputJson.get("json"));

    }
}