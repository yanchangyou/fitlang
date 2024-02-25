package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class AssertJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'assert'," +
                "   'expected': {'message':'hello, world!'}" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'message':'hello, world!'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertEquals("hello, world!", outputJson.getString("message"));

    }


    public void testExecute2() {
        String flow = "{" +//
                "   'uni': 'assert'," +
                "   'expected': {'message':'${message}'}" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'message':'hello, world!'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertEquals("hello, world!", outputJson.getString("message"));

    }

    public void testExecute3() {
        String flow = "{" +//
                "   'uni': 'assert'," +
                "    'needToString': true," +
                "   'expected': {'message':'${message}'}" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'message':'hello, world!'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertEquals("hello, world!", outputJson.getString("message"));

    }

    public void testExecute4() {
        String flow = "{" +//
                "   'uni': 'assert'," +
                "    'needToString': true," +
                "   'expected': {'message':'${message+\"1\"}'}" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'message':'hello, world!'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertFalse(outputJson.getBoolean("success"));

    }

    public void testExecute5() {
        String flow = "{" +//
                "   'uni': 'assert'," +
                "   'expected': '${a}'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'a':{}}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertFalse(outputJson.getBoolean("success"));

    }

    public void testExecuteContainField1() {
        String flow = "{" +//
                "   'uni': 'assert'," +
                "   'expected': {'message':'hello, world!'}," +
                "   'containField': ['message']" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'message':'hello, world!'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertEquals("hello, world!", outputJson.getString("message"));

    }

    public void testExecuteContainField2() {
        String flow = "{" +//
                "   'uni': 'assert'," +
                "   'expected': {'message':'hello, world!'}," +
                "   'containField': ['message1']" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'message':'hello, world!'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertFalse(outputJson.getBoolean("success"));

    }


    public void testExecuteContainJson1() {
        String flow = "{" +//
                "   'uni': 'assert'," +
                "   'containJson': {'hello':'world'}" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'hello':'world','foo':'bar'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertEquals("bar", outputJson.getString("foo"));

    }

    public void testExecuteContainJson2() {
        String flow = "{" +//
                "   'uni': 'assert'," +
                "   'containJson': {'hello':'world1'}" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'hello':'world','foo':'bar'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertFalse(outputJson.getBoolean("success"));

    }
}