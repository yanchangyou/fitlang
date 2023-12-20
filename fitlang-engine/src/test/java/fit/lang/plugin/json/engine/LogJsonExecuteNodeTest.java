package fit.lang.plugin.json.engine;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;

public class LogJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        JsonExecuteContext nodeContext = new JsonExecuteContext();
        JSONObject nodeDefine = JSON.parseObject("{'uni':'print'}");
        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(nodeDefine);
        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);
        input.set("hello", "world");
        executeNode.execute(input, new JsonExecuteNodeOutput(nodeContext));
    }
}