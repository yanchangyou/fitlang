package fit.lang.plugin.object.simple;

import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.object.define.ObjectExecuteContext;
import fit.lang.plugin.object.define.ObjectExecuteNodeData;
import fit.lang.plugin.object.define.ObjectExecuteNodeInput;
import fit.lang.plugin.object.define.ObjectExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class HelloObjectExecuteNodeTest extends TestCase {

    public void testExecute() {

        ExecuteNode executeNode = new HelloObjectExecuteNode();

        ObjectExecuteContext nodeContext = new ObjectExecuteContext();

        ObjectExecuteNodeInput input = new ObjectExecuteNodeInput(nodeContext);
        ObjectExecuteNodeOutput output = new ObjectExecuteNodeOutput(nodeContext);

        input.setNodeData(new ObjectExecuteNodeData("world"));

        executeNode.execute(input, output);

        System.out.println(output.getNodeData().getData());

        Assert.assertEquals(output.getNodeData().getData(), "hello, world!");


    }
}