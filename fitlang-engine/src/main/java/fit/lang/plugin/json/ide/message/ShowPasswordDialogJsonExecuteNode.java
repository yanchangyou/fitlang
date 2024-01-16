package fit.lang.plugin.json.ide.message;

import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.ide.UserIdeManager;

/**
 * 执行节点
 */
public class ShowPasswordDialogJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String title = parseStringField("title", input);
        String message = parseStringField("message", input);

        String password = UserIdeManager.getUserIdeInterface().showPasswordDialog(title, message);

        output.set("password", password);

    }
}