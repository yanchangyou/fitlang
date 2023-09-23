package fit.lang.plugin.map;

import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.map.define.MapExecuteContext;
import fit.lang.plugin.map.define.MapExecuteNodeInput;
import fit.lang.plugin.map.define.MapExecuteNodeOutput;
import fit.lang.plugin.map.simple.HelloMapExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class HelloMapExecuteNodeTest extends TestCase {

    public void testExecute() {

        ExecuteNode executeNode = new HelloMapExecuteNode();

        MapExecuteContext nodeContext = new MapExecuteContext();

        MapExecuteNodeInput input = new MapExecuteNodeInput(nodeContext);
        MapExecuteNodeOutput output = new MapExecuteNodeOutput(nodeContext);

        input.getNodeData().getData().put("who", "world");

        Assert.assertTrue(output.getNodeData().getData().isEmpty());

        executeNode.execute(input, output);

        System.out.println(output.getNodeData().getData());

        Assert.assertTrue(output.getNodeData().getData().containsKey("message"));

        Assert.assertEquals("hello, world!", output.getNodeData().getData().get("message"));


    }
}