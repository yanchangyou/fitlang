package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSON;
import fit.lang.define.ExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class JsonExecuteNodeEngineTest extends TestCase {

    public void testExecute() {

        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(JSON.parseObject("{'uni':'hello'}"));

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.set("who", "world");

        Assert.assertTrue(output.isEmpty());

        executeNode.execute(input, output);

        System.out.println(output.getData());

        Assert.assertTrue(output.containsKey("message"));

        Assert.assertEquals("hello, world!", output.getString("message"));

    }

    public void testExecuteEmpty() {
        try {
            new JsonDynamicFlowExecuteEngine(JSON.parseObject("{}"));
        } catch (Exception e) {
            Assert.assertEquals("define is empty!", e.getMessage());
        }
    }

    public void testExecuteUniEmpty() {
        try {
            new JsonDynamicFlowExecuteEngine(JSON.parseObject("{'uni':''}"));
        } catch (Exception e) {
            Assert.assertEquals("node uni is empty!", e.getMessage());
        }
    }

    public void testExecute1() {

        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(JSON.parseObject("{'uni':'pipe','child':[{'uni':'hello'}]}"));

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.set("who", "world");

        Assert.assertTrue(output.isEmpty());

        executeNode.execute(input, output);

        System.out.println(output.getData());

        Assert.assertTrue(output.containsKey("message"));

        Assert.assertEquals("hello, world!", output.getString("message"));

    }

    public void testExecutePipe1() {

        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(JSON.parseObject("{'uni':'pipe','child':[{'uni':'hello'},{'uni':'print'}]}"));

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.set("who", "world");

        Assert.assertTrue(output.isEmpty());

        executeNode.execute(input, output);

        System.out.println(output.getData());

        Assert.assertTrue(output.containsKey("message"));

        Assert.assertEquals("hello, world!", output.getString("message"));

    }

    public void testExecuteForeach1() {

        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(JSON.parseObject("{'uni':'foreach','foreachField':'array','child':{'uni':'pipe','child':[{'uni':'hello'},{'uni':'print'}]}}"));

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.set("who", "world");
        input.set("array", JSON.parseArray("[{'a':1,'b':'1','who':'world1'},{'a':2,'b':'2','who':'world2'}]"));

        Assert.assertTrue(output.isEmpty());

        executeNode.execute(input, output);

        System.out.println(output.getData());

        Assert.assertTrue(output.containsKey("array"));
        Assert.assertEquals("hello, world2!", output.getJsonArray("array").getJSONObject(1).getString("message"));

    }
}