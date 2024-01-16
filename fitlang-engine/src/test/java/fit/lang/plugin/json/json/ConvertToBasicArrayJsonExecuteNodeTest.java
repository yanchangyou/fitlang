package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ConvertToBasicArrayJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'convertToBasicArray'," +
                "   'arrayField': 'list'," +
                "   'keyField': 'key'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{'list': [{\"key\":1},{\"key\":2},{\"key\":3}]}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("list"));

        Assert.assertEquals("[1,2,3]", outputJson.getString("list"));

    }


    public void testExecute1() {
        String flow = "{" +//
                "   'uni': 'convertToBasicArray'," +
                "   'arrayField': 'parent.list'," +
                "   'keyField': 'foo'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{'parent':{\"list\":[{\"foo\":1},{\"foo\":2},{\"foo\":3}]}}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("parent"));

        Assert.assertEquals("{\"list\":[1,2,3]}", outputJson.getString("parent"));

    }
}