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

        Object page = parseField("page", input);
        Object data = parseField("data", input);

        JSONObject jsonData;
        if (data instanceof JSONObject) {
            jsonData = (JSONObject) data;
        } else {
            jsonData = new JSONObject();
            jsonData.put("data", data);
        }

        JSONObject jsonPage;
        if (page instanceof JSONObject) {
            jsonPage = (JSONObject) page;
        } else {
            jsonPage = new JSONObject();
            jsonPage.put("type", "page");
            jsonPage.put("title", "Json Page");
        }

        JSONObject option = nodeJsonDefine.getJSONObject("option");
        JSONObject context = input.getInputParamAndContextParam();

        jsonData = UserIdeManager.getUserIdeInterface().showJsonPage(jsonPage, jsonData, option, context);

        output.set("data", jsonData);

    }
}
