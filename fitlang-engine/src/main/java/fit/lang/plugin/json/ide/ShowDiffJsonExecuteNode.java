package fit.lang.plugin.json.ide;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ShowDiffJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject json1 = input.getJsonObject("json1");
        JSONObject json2 = input.getJsonObject("json2");
        JSONObject option = nodeJsonDefine.getJSONObject("option");
        JSONObject context = input.getInputParamAndContextParam();

        JSONObject jsonData = UserIdeManager.getUserIdeInterface().showDiff(json1, json2, option, context);

        output.set("data", jsonData);

    }
}
