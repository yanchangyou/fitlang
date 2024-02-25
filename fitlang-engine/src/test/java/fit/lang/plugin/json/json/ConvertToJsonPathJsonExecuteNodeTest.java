package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ConvertToJsonPathJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        Object expected = "{\"number\":\"1\",\"double\":\"1.0\",\"string\":\"abc\",\"object.number\":\"1\",\"object.double\":\"1.0\",\"object.string\":\"abc\",\"array[0].key\":\"1\",\"array[1].key\":\"2\",\"array[2].key\":\"3\"}";
        String flow = "{" +//
                "   'uni': 'convertToJsonPath'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{'number':1,'double':1.0,'string':'abc','object':{'number':1,'double':1.0,'string':'abc'},'array': [{\"key\":1},{\"key\":2},{\"key\":3}]}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertEquals(9, outputJson.size());

        Assert.assertEquals(expected, outputJson.toString());

    }
}