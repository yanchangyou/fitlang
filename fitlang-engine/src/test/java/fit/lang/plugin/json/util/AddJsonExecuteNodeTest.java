package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.json.AddJsonExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class AddJsonExecuteNodeTest extends TestCase {

    public void testAddInt() {

        JSONArray list = new JSONArray();
        list.add(1);
        list.add(2);
        list.add(3);

        Object result = testAdd(list);

        Assert.assertEquals(6, result);
    }

    public void testAddDouble() {

        JSONArray list = new JSONArray();
        list.add(1.0);
        list.add(2.0);
        list.add(3.0);

        Object result = testAdd(list);

        Assert.assertEquals(6.0, result);
    }

    public void testAddString() {

        JSONArray list = new JSONArray();
        list.add("1");
        list.add("2");
        list.add("3");

        Object result = testAdd(list);

        Assert.assertEquals("123", result);
    }

    public void testAddJsonObject() {

        JSONArray list = new JSONArray();
        list.add(JSON.parse("{'a':1}"));
        list.add(JSON.parse("{'b':2}"));
        list.add(JSON.parse("{'c':3}"));

        Object result = testAdd(list);

        Assert.assertEquals(JSON.parse("{\"a\":1,\"b\":2,\"c\":3}"), result);
    }

    public void testAddJsonArray() {

        JSONArray list = new JSONArray();
        list.add(JSON.parse("[{'a':1}]"));
        list.add(JSON.parse("[{'b':2}]"));
        list.add(JSON.parse("[{'c':3}]"));

        Object result = testAdd(list);

        Assert.assertEquals(JSON.parse("[{\"a\":1},{\"b\":2},{\"c\":3}]"), result);
    }

    private static Object testAdd(JSONArray list) {
        ExecuteNode executeNode = new AddJsonExecuteNode();

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.set("list", list);

        Assert.assertTrue(output.isEmpty());

        executeNode.execute(input, output);

        System.out.println(output.getData());

        Assert.assertTrue(output.containsKey("result"));

        return output.get("result");
    }
}