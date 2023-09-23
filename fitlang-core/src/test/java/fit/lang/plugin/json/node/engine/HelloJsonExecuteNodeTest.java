package fit.lang.plugin.json.node.engine;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class HelloJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {

        String flow = "{'uni':'node:hello'}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"message\":\"hello, world!\"}", output);

    }

    public void testExecute1() {

        JSONObject nodeDefine = JSON.parseObject("{'uni':'node:hello'}");
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