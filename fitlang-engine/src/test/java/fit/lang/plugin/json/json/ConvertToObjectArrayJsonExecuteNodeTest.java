package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ConvertToObjectArrayJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'convertToObjectArray'," +
                "   'arrayField': 'list'," +
                "   'keyField': 'key'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{'list': [1,2,3]}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("list"));

        Assert.assertEquals("[{\"key\":1},{\"key\":2},{\"key\":3}]", outputJson.getString("list"));

    }


    public void testExecute1() {
        String flow = "{" +//
                "   'uni': 'convertToObjectArray'," +
                "   'arrayField': 'parent.list'," +
                "   'keyField': 'foo'," +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{'parent':{'list': [1,2,3]}}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("parent"));

        Assert.assertEquals("{\"list\":[{\"foo\":1},{\"foo\":2},{\"foo\":3}]}", outputJson.getString("parent"));

    }
}