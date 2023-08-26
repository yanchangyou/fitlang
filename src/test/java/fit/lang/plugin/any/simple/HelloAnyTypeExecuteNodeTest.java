package fit.lang.plugin.any.simple;

import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.any.define.AnyTypeExecuteContext;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeData;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeInput;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class HelloAnyTypeExecuteNodeTest extends TestCase {

    public void testExecute() {

        ExecuteNode executeNode = new HelloAnyTypeExecuteNode();

        AnyTypeExecuteContext nodeContext = new AnyTypeExecuteContext();

        AnyTypeExecuteNodeInput input = new AnyTypeExecuteNodeInput(nodeContext);
        AnyTypeExecuteNodeOutput output = new AnyTypeExecuteNodeOutput(nodeContext);

        input.setNodeData(new AnyTypeExecuteNodeData<>("world"));

        executeNode.execute(input, output);

        System.out.println(output.getNodeData().getData());

        Assert.assertEquals(output.getNodeData().getData(), "hello, world!");


    }
}