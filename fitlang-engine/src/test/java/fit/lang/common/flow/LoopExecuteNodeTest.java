package fit.lang.common.flow;

import fit.lang.common.util.PrintExecuteNode;
import fit.lang.define.ExecuteNodeOutput;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

import java.util.List;

public class LoopExecuteNodeTest extends TestCase {

    public void testExecute() {

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);
        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.getNodeData().getData().put("who", "world");

        LoopExecuteNode loopExecuteNode = new LoopExecuteNode() {
            @Override
            public List getBags(int size) {
                return null;
            }

            @Override
            public void setBags(String bagsFieldName, List list, ExecuteNodeOutput output) {

            }
        };

        loopExecuteNode.setLoopTimes(2);

        PrintExecuteNode printExecuteNode = new PrintExecuteNode();

        loopExecuteNode.addChildNode(printExecuteNode);

        loopExecuteNode.execute(input, output);

        Assert.assertEquals(2, loopExecuteNode.getCurrentIndex());

    }
}