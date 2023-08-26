package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSON;
import fit.lang.common.util.EchoExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.util.HelloJsonExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class JsonSwitchExecuteNodeTest extends TestCase {

    public void testExecuteCase1() {

        testExecuteCase("1", "message", "hello, world!");

    }

    public void testExecuteCase2() {

        testExecuteCase("2", "who", "world");

    }

    public void testExecuteCase(String type, String outputFieldName, String expect) {

        JsonSwitchExecuteNode executeNode = new JsonSwitchExecuteNode();

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.set("who", "world");
        input.set("type", type);

        HelloJsonExecuteNode helloNode = new HelloJsonExecuteNode();
        EchoExecuteNode echoNode = new EchoExecuteNode();

        helloNode.setNodeDefine(new JsonExecuteNodeData(JSON.parseObject("{'case':'1'}")));
        echoNode.setNodeDefine(new JsonExecuteNodeData(JSON.parseObject("{'case':'2'}")));

        executeNode.addCaseNode("1", helloNode);
        executeNode.addCaseNode("2", echoNode);

        Assert.assertTrue(output.isEmpty());

        executeNode.setSwitchField("type");

        executeNode.execute(input, output);

        System.out.println(output.getData());

        Assert.assertEquals(expect, output.get(outputFieldName));

    }
}