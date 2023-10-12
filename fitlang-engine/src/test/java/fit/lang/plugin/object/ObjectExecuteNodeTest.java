package fit.lang.plugin.object;

import fit.lang.plugin.object.define.ObjectExecuteNodeInput;
import fit.lang.plugin.object.define.ObjectExecuteNodeOutput;
import fit.lang.plugin.object.define.ObjectExecuteContext;
import fit.lang.plugin.object.define.ObjectExecuteNode;
import fit.lang.plugin.object.simple.HelloObjectExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class ObjectExecuteNodeTest extends TestCase {

    public void testExecute() {

        ObjectExecuteNode objectExecuteNode = new HelloObjectExecuteNode();

        ObjectExecuteContext nodeContext = new ObjectExecuteContext();
        ObjectExecuteNodeOutput output = new ObjectExecuteNodeOutput(nodeContext);
        objectExecuteNode.execute(new ObjectExecuteNodeInput(nodeContext), output);

        System.out.println(output.getNodeData().getData());

        Assert.assertEquals("hello, nobody!", output.getNodeData().getData());
    }
}