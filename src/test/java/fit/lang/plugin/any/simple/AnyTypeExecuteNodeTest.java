package fit.lang.plugin.any.simple;

import fit.lang.plugin.any.define.AnyTypeExecuteContext;
import fit.lang.plugin.any.define.AnyTypeExecuteNode;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeInput;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class AnyTypeExecuteNodeTest extends TestCase {

    public void testExecute() {

        AnyTypeExecuteNode anyTypeExecuteNode = new HelloAnyTypeExecuteNode();

        AnyTypeExecuteContext nodeContext = new AnyTypeExecuteContext();
        AnyTypeExecuteNodeOutput output = new AnyTypeExecuteNodeOutput(nodeContext);
        anyTypeExecuteNode.execute(new AnyTypeExecuteNodeInput(nodeContext), output);

        System.out.println(output.getNodeData().getData());

        Assert.assertEquals("hello, nobody!", output.getNodeData().getData());
    }
}