package fit.lang.plugin.json.next;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class JsonExecuteNodeExceptionTest extends TestCase {

    public void testExecuteException() {

        try {
            JSONObject nodeDefine = JSON.parseObject("{'uni':'switch','switchField':'type','child':[]}");
            ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);

            JsonExecuteContext nodeContext = new JsonExecuteContext();

            JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

            JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

            input.set("who", "world");
            input.set("type", "1");

            Assert.assertTrue(output.isEmpty());

            executeNode.execute(input, output);

            System.out.println(output.getData());

            Assert.assertTrue(output.containsKey("who"));

            Assert.assertEquals("world", output.getString("who"));

        } catch (Exception e) {
            e.printStackTrace();
            Assert.assertEquals("switch node case field is empty and child is empty!", e.getCause().getMessage());
        }
    }

    public void testExecuteException2() {

        try {
            JSONObject nodeDefine = JSON.parseObject("{'uni':'switch','id':'node1','case':{},'switchField':'type'}");
            ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);

            JsonExecuteContext nodeContext = new JsonExecuteContext();

            JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

            JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

            input.set("who", "world");
            input.set("type", "1");

            Assert.assertTrue(output.isEmpty());

            executeNode.execute(input, output);

            System.out.println(output.getData());

            Assert.assertTrue(output.containsKey("who"));

            Assert.assertEquals("world", output.getString("who"));

        } catch (Exception e) {
            Assert.assertEquals("switch case node is empty: switch.node1", e.getMessage());
        }
    }
}