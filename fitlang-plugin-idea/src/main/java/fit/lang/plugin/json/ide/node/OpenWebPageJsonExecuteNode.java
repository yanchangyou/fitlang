package fit.lang.plugin.json.ide.node;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class OpenWebPageJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String url = ExecuteJsonNodeUtil.parseStringField("url", input, nodeJsonDefine);

        JSONObject option = nodeJsonDefine.getJSONObject("option");
        JSONObject context = input.getInputParamAndContextParam();

        UserIdeManager.getUserIdeInterface().openWebPage(url, option, context);

    }
}
