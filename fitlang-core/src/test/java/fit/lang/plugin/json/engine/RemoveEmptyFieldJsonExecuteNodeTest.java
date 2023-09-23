package fit.lang.plugin.json.engine;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

import static com.alibaba.fastjson2.JSONWriter.Feature.WriteMapNullValue;

public class RemoveEmptyFieldJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        JSONObject nodeDefine = JSON.parseObject("{'uni':'removeEmptyField'}");
        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.set("who", "world");
        input.set("field1", null);
        input.set("field2", "");

        Assert.assertTrue(output.isEmpty());

        Assert.assertTrue(input.containsKey("field1"));
        Assert.assertTrue(input.containsKey("field2"));

        executeNode.execute(input, output);

        Assert.assertTrue(input.containsKey("field1"));
        Assert.assertTrue(input.containsKey("field2"));

        Assert.assertFalse(output.containsKey("field1"));
        Assert.assertFalse(output.containsKey("field2"));

        System.out.println(input.getData().toString(WriteMapNullValue));
        System.out.println(output.getData());

        Assert.assertEquals("world", output.getString("who"));

    }
}