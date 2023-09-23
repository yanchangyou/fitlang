package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import fit.lang.common.util.PrintExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class JsonForeachExecuteNodeTest extends TestCase {

    public void testExecute() {

        JsonForeachExecuteNode executeNode = new JsonForeachExecuteNode();

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        JSONArray list = JSON.parseArray("[{\"a\":1},{\"b\":2},{\"c\":3}]");

        input.set("list", list);

        Assert.assertTrue(output.isEmpty());

        executeNode.addChildNode(new PrintExecuteNode());

        executeNode.execute(input, output);

        System.out.println(output.getData());


    }
}