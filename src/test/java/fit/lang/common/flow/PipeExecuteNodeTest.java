package fit.lang.common.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.common.util.EchoExecuteNode;
import fit.lang.common.util.PrintExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.util.HelloJsonExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class PipeExecuteNodeTest extends TestCase {

    public void testExecute() {

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);
        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.getNodeData().getData().put("who", "world");

        Object oldInput = input.getData();

        PipeExecuteNode pipeExecuteNode = new PipeExecuteNode();

        PrintExecuteNode printExecuteNode = new PrintExecuteNode();
        EchoExecuteNode echoNode = new EchoExecuteNode();
        HelloJsonExecuteNode helloExecuteNode = new HelloJsonExecuteNode();

        helloExecuteNode.setNodeDefine(JSONObject.parseObject("{'uni':'hello'}"));

        pipeExecuteNode.addChildNode(printExecuteNode);
        pipeExecuteNode.addChildNode(echoNode);
        pipeExecuteNode.addChildNode(printExecuteNode);
        pipeExecuteNode.addChildNode(helloExecuteNode);
        pipeExecuteNode.addChildNode(printExecuteNode);

        pipeExecuteNode.execute(input, output);

        Assert.assertEquals(output.getNodeData().getData().get("message"), "hello, world!");

        Object newInput = input.getData();

        Assert.assertEquals(oldInput, newInput);
    }

    public void testExecuteKeepInput() {

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);
        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.getNodeData().getData().put("who", "world");

        Object oldInput = input.getData();

        PipeExecuteNode pipeExecuteNode = new PipeExecuteNode();
        pipeExecuteNode.setNeedCloneInputData(true);

        //TODO ExecuteNode
        PrintExecuteNode printExecuteNode = new PrintExecuteNode();
        EchoExecuteNode echoNode = new EchoExecuteNode();
        HelloJsonExecuteNode helloExecuteNode = new HelloJsonExecuteNode();
        helloExecuteNode.setNodeDefine(JSONObject.parseObject("{'uni':'hello'}"));

        pipeExecuteNode.addChildNode(printExecuteNode);
        pipeExecuteNode.addChildNode(echoNode);
        pipeExecuteNode.addChildNode(printExecuteNode);
        pipeExecuteNode.addChildNode(helloExecuteNode);
        pipeExecuteNode.addChildNode(printExecuteNode);

        pipeExecuteNode.execute(input, output);

        Assert.assertEquals(output.get("message"), "hello, world!");

        Object newInput = input.getData();

        Assert.assertNotEquals(oldInput, newInput);

    }
}