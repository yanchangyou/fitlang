package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ConvertObjectToArrayJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'convertObjectToArray'," +
                "   'objectField': 'object'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'object':" +
                "{'key':'name','value':'cat'}," +
                "}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("array"));

        Assert.assertEquals("[{\"key\":\"key\",\"value\":\"name\"},{\"key\":\"value\",\"value\":\"cat\"}]", outputJson.getString("array"));

    }

    public void testExecute1() {
        String flow = "{" +//
                "   'uni': 'convertObjectToArray'," +
                "   'objectField': 'object'," +
                "   'keyField': 'foo'," +
                "   'valueField': 'bar'," +
                "   'arrayField': 'list'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'object':" +
                "{'key':'name','value':'cat'}," +
                "}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("list"));

        Assert.assertEquals("[{\"foo\":\"key\",\"bar\":\"name\"},{\"foo\":\"value\",\"bar\":\"cat\"}]", outputJson.getString("list"));

    }
}