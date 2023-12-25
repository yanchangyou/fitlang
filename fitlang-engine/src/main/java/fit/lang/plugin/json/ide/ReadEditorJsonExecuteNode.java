package fit.lang.plugin.json.ide;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;

/**
 * 执行节点
 */
public class ReadEditorJsonExecuteNode extends JsonExecuteNode {


    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String content = UserIdeManager.getUserIdeInterface().readEditorContent();

        if (isJsonObjectText(content)) {

            output.setData(JSONObject.parse(content));

        } else {
            String contentField = parseStringField("contentField", input);
            if (contentField == null) {
                contentField = "content";
            }
            output.set(contentField, content);
        }
    }
}
