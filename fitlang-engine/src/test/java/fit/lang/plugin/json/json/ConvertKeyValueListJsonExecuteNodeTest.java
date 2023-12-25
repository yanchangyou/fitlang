package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ConvertKeyValueListJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'convertKeyValueList'," +
                "   'jsonField': 'json'," +
                "   'listField': 'list'," +
                "   'keyField': 'key'," +
                "   'valueField': 'value'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{ 'list': [" +
                "{'key':'name','value':'cat'}," +
                "{'key':'code','value':'001'}," +
                "]}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("list"));

        Assert.assertEquals("{\"name\":\"cat\",\"code\":\"001\"}", outputJson.getString("list"));

    }
}