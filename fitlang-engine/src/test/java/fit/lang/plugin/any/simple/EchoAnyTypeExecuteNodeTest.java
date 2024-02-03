package fit.lang.plugin.any.simple;

import fit.lang.define.ExecuteNode;
import fit.lang.plugin.any.define.AnyTypeExecuteContext;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeData;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeInput;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class EchoAnyTypeExecuteNodeTest extends TestCase {

    public void testExecute() {

        ExecuteNode executeNode = new EchoAnyTypeExecuteNode();

        AnyTypeExecuteContext nodeContext = new AnyTypeExecuteContext();

        AnyTypeExecuteNodeInput input = new AnyTypeExecuteNodeInput(nodeContext);
        AnyTypeExecuteNodeOutput output = new AnyTypeExecuteNodeOutput(nodeContext);

        input.setNodeData(new AnyTypeExecuteNodeData<>("hello, world!"));

        executeNode.execute(input, output);

        System.out.println(output.getNodeData().getData());

        Assert.assertEquals(output.getNodeData().getData(), "hello, world!");


    }
}