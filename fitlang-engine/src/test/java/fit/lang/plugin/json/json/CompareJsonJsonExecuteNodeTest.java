package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class CompareJsonJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        JSONObject input = JSONObject.parseObject("{" +
                "'json1':{'number':1,'double':1.0,'string':'abc','object':{'number':1,'double':1.0,'string':'abc'},'array': [{\"key\":1},{\"key\":2},{\"key\":3}]}," +
                "'json2':{'number':1,'double':1.0,'string':'abc','object':{'number':1,'double':1.0,'string':'abc'},'array': [{\"key\":1},{\"key\":2},{\"key\":3}]}," +
                "}");
        Object expected = "{\n" +
                "\t\"equal\":true,\n" +
                "\t\"total\":0,\n" +
                "\t\"valueEqualCount\":0,\n" +
                "\t\"typeEqualCount\":0,\n" +
                "\t\"diffCount\":0,\n" +
                "\t\"addCount\":0,\n" +
                "\t\"removeCount\":0,\n" +
                "\t\"modifyCount\":0,\n" +
                "\t\"result\":{\n" +
                "\t\t\"number\":{\n" +
                "\t\t\t\"path\":\"number\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"double\":{\n" +
                "\t\t\t\"path\":\"double\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"string\":{\n" +
                "\t\t\t\"path\":\"string\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"object.number\":{\n" +
                "\t\t\t\"path\":\"object.number\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"object.double\":{\n" +
                "\t\t\t\"path\":\"object.double\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"object.string\":{\n" +
                "\t\t\t\"path\":\"object.string\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"array[0].key\":{\n" +
                "\t\t\t\"path\":\"array[0].key\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"array[1].key\":{\n" +
                "\t\t\t\"path\":\"array[1].key\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"array[2].key\":{\n" +
                "\t\t\t\"path\":\"array[2].key\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t}\n" +
                "\t}\n" +
                "}";
        String flow = "{" +//
                "   'uni': 'compare'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode(input.toJSONString(), flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);


        Assert.assertEquals(expected, outputJson.toString(JSONWriter.Feature.PrettyFormat));

    }

    public void testExecute1() {
        JSONObject input = JSONObject.parseObject("{" +
                "'json1':{\"string\":123,\"object\":{\"number\":\"abc\",\"string\":123},\"array\":[{\"number\":\"abc\",\"string\":123}]}," +
                "'json2':{\"number\":1,\"array\":[{\"number\":123,\"string\":\"abc\"}]}" +
                "}");
        Object expected = "{\n" +
                "\t\"equal\":false,\n" +
                "\t\"total\":6,\n" +
                "\t\"valueEqualCount\":0,\n" +
                "\t\"typeEqualCount\":2,\n" +
                "\t\"diffCount\":6,\n" +
                "\t\"addCount\":1,\n" +
                "\t\"removeCount\":3,\n" +
                "\t\"modifyCount\":2,\n" +
                "\t\"result\":{\n" +
                "\t\t\"string\":{\n" +
                "\t\t\t\"path\":\"string\",\n" +
                "\t\t\t\"valueEqual\":false,\n" +
                "\t\t\t\"diffType\":\"REMOVE\",\n" +
                "\t\t\t\"value1\":123\n" +
                "\t\t},\n" +
                "\t\t\"object.number\":{\n" +
                "\t\t\t\"path\":\"object.number\",\n" +
                "\t\t\t\"valueEqual\":false,\n" +
                "\t\t\t\"diffType\":\"REMOVE\",\n" +
                "\t\t\t\"value1\":\"abc\"\n" +
                "\t\t},\n" +
                "\t\t\"object.string\":{\n" +
                "\t\t\t\"path\":\"object.string\",\n" +
                "\t\t\t\"valueEqual\":false,\n" +
                "\t\t\t\"diffType\":\"REMOVE\",\n" +
                "\t\t\t\"value1\":123\n" +
                "\t\t},\n" +
                "\t\t\"array[0].number\":{\n" +
                "\t\t\t\"path\":\"array[0].number\",\n" +
                "\t\t\t\"valueEqual\":false,\n" +
                "\t\t\t\"typeEqual\":false,\n" +
                "\t\t\t\"diffType\":\"MODIFY\",\n" +
                "\t\t\t\"value1\":\"abc\",\n" +
                "\t\t\t\"value2\":123\n" +
                "\t\t},\n" +
                "\t\t\"array[0].string\":{\n" +
                "\t\t\t\"path\":\"array[0].string\",\n" +
                "\t\t\t\"valueEqual\":false,\n" +
                "\t\t\t\"typeEqual\":false,\n" +
                "\t\t\t\"diffType\":\"MODIFY\",\n" +
                "\t\t\t\"value1\":123,\n" +
                "\t\t\t\"value2\":\"abc\"\n" +
                "\t\t},\n" +
                "\t\t\"number\":{\n" +
                "\t\t\t\"path\":\"number\",\n" +
                "\t\t\t\"valueEqual\":false,\n" +
                "\t\t\t\"diffType\":\"ADD\",\n" +
                "\t\t\t\"value2\":1\n" +
                "\t\t}\n" +
                "\t}\n" +
                "}";
        String flow = "{" +//
                "   'uni': 'compare'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode(input.toJSONString(), flow);
        System.out.println(output);

        JSONObject outputJson = JSON.parseObject(output);
        System.out.println(outputJson.toJSONString(JSONWriter.Feature.PrettyFormat));

        Assert.assertEquals(expected, outputJson.toString(JSONWriter.Feature.PrettyFormat));

    }

    public void testExecute2() {
        JSONObject input = JSONObject.parseObject("{" +
                "'json1':{'number':123,'double':1.230,'string':'abcd','object':{'number':1,'double':1.0,'string':'abc'},'array': [{\"key\":1},{\"key\":2},{\"key\":3}]}," +
                "'json2':{'number':1,'double':1.0,'string':'abc','object':{'number':1,'double':1.0,'string':'abc'},'array': [{\"key\":1},{\"key\":2},{\"key\":3}]}," +
                "}");
        Object expected = "{\n" +
                "\t\"equal\":false,\n" +
                "\t\"total\":3,\n" +
                "\t\"valueEqualCount\":0,\n" +
                "\t\"typeEqualCount\":0,\n" +
                "\t\"diffCount\":3,\n" +
                "\t\"addCount\":0,\n" +
                "\t\"removeCount\":0,\n" +
                "\t\"modifyCount\":3,\n" +
                "\t\"result\":{\n" +
                "\t\t\"number\":{\n" +
                "\t\t\t\"path\":\"number\",\n" +
                "\t\t\t\"valueEqual\":false,\n" +
                "\t\t\t\"typeEqual\":true,\n" +
                "\t\t\t\"diffType\":\"MODIFY\",\n" +
                "\t\t\t\"value1\":123,\n" +
                "\t\t\t\"value2\":1\n" +
                "\t\t},\n" +
                "\t\t\"double\":{\n" +
                "\t\t\t\"path\":\"double\",\n" +
                "\t\t\t\"valueEqual\":false,\n" +
                "\t\t\t\"typeEqual\":true,\n" +
                "\t\t\t\"diffType\":\"MODIFY\",\n" +
                "\t\t\t\"value1\":1.230,\n" +
                "\t\t\t\"value2\":1.0\n" +
                "\t\t},\n" +
                "\t\t\"string\":{\n" +
                "\t\t\t\"path\":\"string\",\n" +
                "\t\t\t\"valueEqual\":false,\n" +
                "\t\t\t\"typeEqual\":true,\n" +
                "\t\t\t\"diffType\":\"MODIFY\",\n" +
                "\t\t\t\"value1\":\"abcd\",\n" +
                "\t\t\t\"value2\":\"abc\"\n" +
                "\t\t},\n" +
                "\t\t\"object.number\":{\n" +
                "\t\t\t\"path\":\"object.number\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"object.double\":{\n" +
                "\t\t\t\"path\":\"object.double\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"object.string\":{\n" +
                "\t\t\t\"path\":\"object.string\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"array[0].key\":{\n" +
                "\t\t\t\"path\":\"array[0].key\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"array[1].key\":{\n" +
                "\t\t\t\"path\":\"array[1].key\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t},\n" +
                "\t\t\"array[2].key\":{\n" +
                "\t\t\t\"path\":\"array[2].key\",\n" +
                "\t\t\t\"valueEqual\":true\n" +
                "\t\t}\n" +
                "\t}\n" +
                "}";
        String flow = "{" +//
                "   'uni': 'compare'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode(input.toJSONString(), flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(outputJson.toString(JSONWriter.Feature.PrettyFormat));


        Assert.assertEquals(expected, outputJson.toString(JSONWriter.Feature.PrettyFormat));

    }
}