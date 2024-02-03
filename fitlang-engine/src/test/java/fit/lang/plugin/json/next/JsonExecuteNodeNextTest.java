package fit.lang.plugin.json.next;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.ExecuteNode;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class JsonExecuteNodeNextTest extends TestCase {

    public void testExecute() {

        JSONObject nodeDefine = JSON.parseObject("{'uni':'echo','next':{'uni':'hello'}}");
        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);

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

    public void testExecute2() {

        JSONObject nodeDefine = JSON.parseObject("{'uni':'echo','next':[{'uni':'hello'}]}");
        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);

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

    public void testExecute3() {

        JSONObject nodeDefine = JSON.parseObject("{'uni':'print','next':{'uni':'echo','next':{'uni':'print','next':{'uni':'hello'}}}}");
        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);

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

    public void testExecute4() {

        JSONObject nodeDefine = JSON.parseObject("{'uni':'switch','switchField':'type','child':[{'case':'1','uni':'echo'},{'case':'2','uni':'hello'}]}");
        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.set("who", "world");
        input.set("type", "2");

        Assert.assertTrue(output.isEmpty());

        executeNode.execute(input, output);

        System.out.println(output.getData());

        Assert.assertTrue(output.containsKey("message"));

        Assert.assertEquals("hello, world!", output.getString("message"));

    }

    public void testExecute5() {

        JSONObject nodeDefine = JSON.parseObject("{'uni':'switch','switchField':'type','child':[{'case':'1','uni':'echo'},{'case':'2','uni':'hello'}]}");
        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.set("who", "world");
        input.set("type", "1");

        Assert.assertTrue(output.isEmpty());

        executeNode.execute(input, output);

        System.out.println(output.getData());

        Assert.assertTrue(output.containsKey("who"));

        Assert.assertEquals("world", output.getString("who"));

    }

    public void testExecuteNextMode() {

        JSONObject nodeDefine = JSON.parseObject("{'uni':'echo','nextMode':'pipe','next':{'uni':'hello'}}");
        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);

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

    public void testExecuteNeedClone() {

        JSONObject nodeDefine = JSON.parseObject("{'uni':'echo','nextMode':'pipe', 'needCloneInputData':true,'next':{'uni':'hello'}}");
        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);

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

}