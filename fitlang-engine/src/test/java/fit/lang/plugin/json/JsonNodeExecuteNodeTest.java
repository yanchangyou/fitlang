package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSON;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class JsonNodeExecuteNodeTest extends TestCase {

    public void testExecute() {

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonNodeExecuteNode executeNode = new JsonNodeExecuteNode(nodeContext);
        executeNode.setNodePath("hello");
        executeNode.setNodeDefine(new JsonExecuteNodeData(JSON.parseObject("{'uni':'node:hello'}")));

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.set("who", "world");

        executeNode.execute(input, output);

        System.out.println(output);

        Assert.assertEquals("hello, world!", output.getString("message"));
    }
}