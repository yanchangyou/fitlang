package fit.lang.plugin.json.ide;

import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class WriteEditorJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String contentField = parseStringField("contentField", input);
        if (contentField == null) {
            contentField = "content";
        }

        String content = input.getString(contentField);

        UserIdeManager.getUserIdeInterface().writeEditorContent(content);

        output.setData(input.getData());

    }
}
