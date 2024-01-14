package fit.lang.plugin.json.ide.message;

import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.ide.UserIdeManager;

/**
 * 执行节点
 */
public class ShowCheckboxOkCancelDialogJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String title = parseStringField("title", input);
        String message = parseStringField("message", input);
        String checkboxText = parseStringField("checkboxText", input);

        int checkIndex = UserIdeManager.getUserIdeInterface().showCheckboxOkCancelDialog(title, message, checkboxText);
        output.set("checkIndex", checkIndex);

    }
}
