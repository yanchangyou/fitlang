package fit.lang.plugin.object.simple;

import fit.lang.define.ExecuteNode;
import fit.lang.plugin.object.define.ObjectExecuteContext;
import fit.lang.plugin.object.define.ObjectExecuteNodeData;
import fit.lang.plugin.object.define.ObjectExecuteNodeInput;
import fit.lang.plugin.object.define.ObjectExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class EchoObjectExecuteNodeTest extends TestCase {

    public void testExecute() {

        ExecuteNode executeNode = new EchoObjectExecuteNode();

        ObjectExecuteContext nodeContext = new ObjectExecuteContext();

        ObjectExecuteNodeInput input = new ObjectExecuteNodeInput(nodeContext);
        ObjectExecuteNodeOutput output = new ObjectExecuteNodeOutput(nodeContext);

        input.setNodeData(new ObjectExecuteNodeData("hello, world!"));

        executeNode.execute(input, output);

        System.out.println(output.getNodeData().getData());

        Assert.assertEquals(output.getNodeData().getData(), "hello, world!");


    }
}