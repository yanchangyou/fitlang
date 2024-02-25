package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class DiffJsonJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        JSONObject input = JSONObject.parseObject("{" +
                "'json1':{'number':1,'double':1.0,'string':'abc','object':{'number':1,'double':1.0,'string':'abc'},'array': [{\"key\":1},{\"key\":2},{\"key\":3}]}," +
                "'json2':{'number':1,'double':1.0,'string':'abc','object':{'number':1,'double':1.0,'string':'abc'},'array': [{\"key\":1},{\"key\":2},{\"key\":3}]}," +
                "}");
        Object expected = "{}";
        String flow = "{" +//
                "   'uni': 'diff'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode(input.toJSONString(), flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);


        Assert.assertEquals(expected, outputJson.toString());

    }

    public void testExecute1() {
        JSONObject input = JSONObject.parseObject("{" +
                "'json1':{\"string\":123,\"object\":{\"number\":\"abc\",\"string\":123},\"array\":[{\"number\":\"abc\",\"string\":123}]}," +
                "'json2':{\"number\":1,\"array\":[{\"number\":123,\"string\":\"abc\"}]}" +
                "}");
        Object expected = "{\"object.string\":{\"equal\":false,\"type\":\"REMOVE\"},\"number\":{\"equal\":false,\"type\":\"ADD\"},\"string\":{\"equal\":false,\"type\":\"REMOVE\"},\"array[0].number\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"array[0].string\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"object.number\":{\"equal\":false,\"type\":\"REMOVE\"}}";
        String flow = "{" +//
                "   'uni': 'diff'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode(input.toJSONString(), flow);
        System.out.println(output);

        JSONObject outputJson = JSON.parseObject(output);
        System.out.println(outputJson.toJSONString(JSONWriter.Feature.PrettyFormat));

        Assert.assertEquals(expected, outputJson.toString());

    }

    public void testExecute2() {
        JSONObject input = JSONObject.parseObject("{" +
                "'json1':{'number':11,'double':1.10,'string':'abcd','object':{'number':1,'double':1.0,'string':'abc'},'array': [{\"key\":1},{\"key\":2},{\"key\":3}]}," +
                "'json2':{'number':1,'double':1.0,'string':'abc','object':{'number':1,'double':1.0,'string':'abc'},'array': [{\"key\":1},{\"key\":2},{\"key\":3}]}," +
                "}");
        Object expected = "{\"number\":{\"equal\":false,\"typeEqual\":true,\"type\":\"MODIFY\"},\"string\":{\"equal\":false,\"typeEqual\":true,\"type\":\"MODIFY\"},\"double\":{\"equal\":false,\"typeEqual\":true,\"type\":\"MODIFY\"}}";
        String flow = "{" +//
                "   'uni': 'diff'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode(input.toJSONString(), flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);


        Assert.assertEquals(expected, outputJson.toString());

    }
}