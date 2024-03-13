package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.ExecuteNode;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class SwitchJsonExecuteNodeTest extends TestCase {

    public void testExecuteCase1() {

        testExecuteCase("1", "message", "hello, world!");

    }

    public void testExecuteCase2() {

        testExecuteCase("2", "who", "world");

    }

    private void testExecuteCase(String type, String outputFieldName, String expect) {

        JSONObject nodeDefine = JSON.parseObject("{'uni':'switch','switchField':'type','case':{'1':{'uni':'hello'},'2':{'uni':'echo'}}}");
        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);
//
        input.set("who", "world");
        input.set("type", type);
//
//        executeNode.addCaseNode("1", new HelloJsonExecuteNode());
//        executeNode.addCaseNode("2", new EchoExecuteNode());

        Assert.assertTrue(output.isEmpty());

//        executeNode.setswitchField("type");

        executeNode.execute(input, output);

        System.out.println(output.getData());

        Assert.assertEquals(expect, output.get(outputFieldName));

    }
}