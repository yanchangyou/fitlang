package fit.lang.plugin.json.ide;

import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ReadEditorJsonExecuteNode extends JsonExecuteNode {


    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String content = UserIdeManager.getUserIdeInterface().readEditorContent();

        String contentField = parseStringField("contentField", input);
        if (contentField == null) {
            contentField = "content";
        }

        output.set(contentField, content);

    }
}
