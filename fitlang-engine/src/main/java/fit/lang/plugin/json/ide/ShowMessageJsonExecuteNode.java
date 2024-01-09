package fit.lang.plugin.json.ide;

import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ShowMessageJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String title = parseStringField("title", input);
        String message = parseStringField("message", input);

        UserIdeManager.getUserIdeInterface().showMessage(0, title, message);

    }
}
