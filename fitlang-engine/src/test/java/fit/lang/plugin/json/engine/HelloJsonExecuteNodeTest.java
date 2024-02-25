package fit.lang.plugin.json.engine;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.ExecuteNode;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class HelloJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        JSONObject nodeDefine = JSON.parseObject("{'uni':'hello'}");
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