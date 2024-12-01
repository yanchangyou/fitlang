package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
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

    public void testExecute1() {
        String flow = "{" +//
                "   'uni': 'convertArrayToObject'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{ 'array': [" +
                "{'key':'name','value':'cat'}," +
                "{'key':'code','value':'001'}," +
                "]}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("array"));

        Assert.assertEquals("{\"name\":{\"key\":\"name\",\"value\":\"cat\"},\"code\":{\"key\":\"code\",\"value\":\"001\"}}", outputJson.getString("array"));

    }

    public void testExecute2() {
        String flow = "{" +//
                "   'uni': 'convertArrayToObject'," +
                "   'extKeyFields': ['value']," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{ 'array': [" +
                "{'key':'name','value':'cat'}," +
                "{'key':'code','value':'001'}," +
                "]}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("array"));

        Assert.assertEquals("{\n" +
                "\t\"array\":{\n" +
                "\t\t\"name_value\":{\n" +
                "\t\t\t\"key\":\"name\",\n" +
                "\t\t\t\"value\":\"cat\"\n" +
                "\t\t},\n" +
                "\t\t\"code_value\":{\n" +
                "\t\t\t\"key\":\"code\",\n" +
                "\t\t\t\"value\":\"001\"\n" +
                "\t\t}\n" +
                "\t}\n" +
                "}", outputJson.toString(JSONWriter.Feature.PrettyFormat))
        ;
    }

    public void testExecute3() {
        String flow = "{" +//
                "   'uni': 'convertArrayToObject'," +
                "   'keyField': 'value'," +
                "   'extKeyFields': ['key']," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{ 'array': [" +
                "{'key':'name','value':'cat'}," +
                "{'key':'code','value':'001'}," +
                "]}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("array"));

        Assert.assertEquals("{\n" +
                "\t\"array\":{\n" +
                "\t\t\"cat_name\":{\n" +
                "\t\t\t\"key\":\"name\",\n" +
                "\t\t\t\"value\":\"cat\"\n" +
                "\t\t},\n" +
                "\t\t\"f_001_code\":{\n" +
                "\t\t\t\"key\":\"code\",\n" +
                "\t\t\t\"value\":\"001\"\n" +
                "\t\t}\n" +
                "\t}\n" +
                "}", outputJson.toString(JSONWriter.Feature.PrettyFormat))
        ;
    }
}