package fit.lang.common.flow;

import fit.lang.common.util.PrintExecuteNode;
import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.util.HelloJsonExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class ThreadExecuteNodeTest extends TestCase {

    public void testExecute() {

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);
        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.getNodeData().getData().put("who", "world");

        ExecuteNode executeNode = new ThreadExecuteNode();

        PrintExecuteNode printExecuteNode = new PrintExecuteNode();
        HelloJsonExecuteNode helloExecuteNode = new HelloJsonExecuteNode();

        executeNode.addChildNode(printExecuteNode);
        executeNode.addChildNode(helloExecuteNode);
        executeNode.addChildNode(printExecuteNode);

        executeNode.execute(input, output);

        Assert.assertEquals("world", output.getNodeData().getData().get("who"));

    }
}