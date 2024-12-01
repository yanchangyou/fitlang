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
        Object expected = "{\"equal\":true,\"total\":9,\"valueEqualCount\":9,\"typeEqualCount\":0,\"diffCount\":0,\"addCount\":0,\"removeCount\":0,\"modifyCount\":0,\"result\":{\"number\":{\"path\":\"number\",\"valueEqual\":true},\"double\":{\"path\":\"double\",\"valueEqual\":true},\"string\":{\"path\":\"string\",\"valueEqual\":true},\"object.number\":{\"path\":\"object.number\",\"valueEqual\":true},\"object.double\":{\"path\":\"object.double\",\"valueEqual\":true},\"object.string\":{\"path\":\"object.string\",\"valueEqual\":true},\"array[0].key\":{\"path\":\"array[0].key\",\"valueEqual\":true},\"array[1].key\":{\"path\":\"array[1].key\",\"valueEqual\":true},\"array[2].key\":{\"path\":\"array[2].key\",\"valueEqual\":true}}}";
        String flow = "{" +//
                "   'uni': 'compare'," +
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
        Object expected = "{\"equal\":false,\"total\":5,\"valueEqualCount\":0,\"typeEqualCount\":0,\"diffCount\":5,\"addCount\":1,\"removeCount\":2,\"modifyCount\":2,\"result\":{\"string\":{\"path\":\"string\",\"valueEqual\":false,\"diffType\":\"REMOVE\",\"value1\":123},\"object\":{\"path\":\"object\",\"valueEqual\":false,\"diffType\":\"REMOVE\",\"value1\":{\"number\":\"abc\",\"string\":123}},\"array[0].number\":{\"path\":\"array[0].number\",\"valueEqual\":false,\"typeEqual\":false,\"diffType\":\"MODIFY\",\"value1\":\"abc\",\"value2\":123},\"array[0].string\":{\"path\":\"array[0].string\",\"valueEqual\":false,\"typeEqual\":false,\"diffType\":\"MODIFY\",\"value1\":123,\"value2\":\"abc\"},\"number\":{\"path\":\"number\",\"valueEqual\":false,\"diffType\":\"ADD\",\"value2\":1}}}";
        String flow = "{" +//
                "   'uni': 'compare'," +
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
        Object expected = "{\"equal\":false,\"total\":9,\"valueEqualCount\":6,\"typeEqualCount\":3,\"diffCount\":3,\"addCount\":0,\"removeCount\":0,\"modifyCount\":3,\"result\":{\"number\":{\"path\":\"number\",\"valueEqual\":false,\"typeEqual\":true,\"diffType\":\"MODIFY\",\"value1\":11,\"value2\":1},\"double\":{\"path\":\"double\",\"valueEqual\":false,\"typeEqual\":true,\"diffType\":\"MODIFY\",\"value1\":1.10,\"value2\":1.0},\"string\":{\"path\":\"string\",\"valueEqual\":false,\"typeEqual\":true,\"diffType\":\"MODIFY\",\"value1\":\"abcd\",\"value2\":\"abc\"},\"object.number\":{\"path\":\"object.number\",\"valueEqual\":true},\"object.double\":{\"path\":\"object.double\",\"valueEqual\":true},\"object.string\":{\"path\":\"object.string\",\"valueEqual\":true},\"array[0].key\":{\"path\":\"array[0].key\",\"valueEqual\":true},\"array[1].key\":{\"path\":\"array[1].key\",\"valueEqual\":true},\"array[2].key\":{\"path\":\"array[2].key\",\"valueEqual\":true}}}";
        String flow = "{" +//
                "   'uni': 'compare'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode(input.toJSONString(), flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);


        Assert.assertEquals(expected, outputJson.toString());

    }
}