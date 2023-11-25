package fit.lang.plugin.json.json;

import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.json.RemoveFieldJsonExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class RemoveFieldJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        RemoveFieldJsonExecuteNode executeNode = new RemoveFieldJsonExecuteNode();
        executeNode.addRemoveField("field1");

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.set("who", "world");
        input.set("field1", "world");

        Assert.assertTrue(output.isEmpty());
        Assert.assertTrue(input.containsKey("field1"));

        executeNode.execute(input, output);

        Assert.assertTrue(input.containsKey("field1"));
        Assert.assertFalse(output.containsKey("field1"));

        System.out.println(output.getData());

        Assert.assertEquals("world", output.getString("who"));

    }
}