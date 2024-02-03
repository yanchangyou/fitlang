package fit.lang.plugin.json.ide.node;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ShowHtmlJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String html = parseStringField("html", input);

        JSONObject option = nodeJsonDefine.getJSONObject("option");
        JSONObject context = input.getInputParamAndContextParam();

        UserIdeManager.getUserIdeInterface().showHtml(html, option, context);

    }
}
