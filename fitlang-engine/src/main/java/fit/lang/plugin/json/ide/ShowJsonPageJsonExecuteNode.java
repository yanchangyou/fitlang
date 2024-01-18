package fit.lang.plugin.json.ide;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ShowJsonPageJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject jsonPage = nodeJsonDefine.getJSONObject("page");
        Object jsonData = parseField("data", input);
        JSONObject json;
        if (jsonData instanceof JSONObject) {
            json = (JSONObject) jsonData;
        } else {
            json = new JSONObject();
            json.put("data", jsonData);
        }

        JSONObject option = nodeJsonDefine.getJSONObject("option");
        JSONObject context = input.getInputParamAndContextParam();

        UserIdeManager.getUserIdeInterface().showJsonPage(jsonPage, json, option, context);

    }
}
