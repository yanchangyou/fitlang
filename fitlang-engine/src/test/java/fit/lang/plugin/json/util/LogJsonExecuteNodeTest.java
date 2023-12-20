package fit.lang.plugin.json.util;

import fit.lang.common.util.PrintExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;

public class LogJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        PrintExecuteNode printExecuteNode = new PrintExecuteNode();
        JsonExecuteContext nodeContext = new JsonExecuteContext();
        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);
        input.set("hello", "world");
        printExecuteNode.execute(input, new JsonExecuteNodeOutput(nodeContext));
    }
}