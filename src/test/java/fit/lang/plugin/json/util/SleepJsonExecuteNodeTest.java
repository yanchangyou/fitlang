package fit.lang.plugin.json.util;

import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;

public class SleepJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        SleepJsonExecuteNode executeNode = new SleepJsonExecuteNode();

        executeNode.setMillis(1000);

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.set("who", "world");

        executeNode.execute(input, output);

    }
}