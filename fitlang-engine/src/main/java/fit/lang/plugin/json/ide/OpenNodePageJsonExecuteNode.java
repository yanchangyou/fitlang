package fit.lang.plugin.json.ide;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class OpenNodePageJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        UserIdeManager.getUserIdeInterface().openNodePage("Test Button", JSONObject.parse("{'uni':'hello'}"), new JSONObject());

    }
}
