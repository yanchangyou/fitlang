package fit.lang.plugin.json.node.engine;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class RemoveFieldJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        JSONObject nodeDefine = JSON.parseObject("{'uni':'node:removeField','fieldNames':['field1']}");
        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);
//        executeNode.addRemoveField("field1");

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