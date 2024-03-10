package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ConvertArrayToObjectJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'convertArrayToObject'," +
                "   'objectField': 'json'," +
                "   'arrayField': 'list'," +
                "   'keyField': 'key'," +
                "   'valueField': 'value'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{ 'list': [" +
                "{'key':'name','value':'cat'}," +
                "{'key':'code','value':'001'}," +
                "]}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("json"));

        Assert.assertEquals("{\"name\":\"cat\",\"code\":\"001\"}", outputJson.getString("json"));

    }
}