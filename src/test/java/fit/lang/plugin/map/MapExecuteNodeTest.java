package fit.lang.plugin.map;

import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.map.define.MapExecuteContext;
import fit.lang.plugin.map.define.MapExecuteNodeInput;
import fit.lang.plugin.map.define.MapExecuteNodeOutput;
import fit.lang.plugin.map.simple.EchoMapExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class MapExecuteNodeTest extends TestCase {

    public void testExecute() {

        ExecuteNode executeNode = new EchoMapExecuteNode();

        MapExecuteContext nodeContext = new MapExecuteContext();

        MapExecuteNodeInput input = new MapExecuteNodeInput(nodeContext);
        MapExecuteNodeOutput output = new MapExecuteNodeOutput(nodeContext);

        input.getNodeData().getData().put("foo", "bar");

        Assert.assertTrue(output.getNodeData().getData().isEmpty());

        executeNode.execute(input, output);

        System.out.println(output.getNodeData().getData());

        Assert.assertEquals("bar", output.getNodeData().getData().get("foo"));

    }
}