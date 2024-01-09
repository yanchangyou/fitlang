package fit.lang.plugin.json.ide.message;

import cn.hutool.core.util.StrUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.ide.UserIdeManager;

/**
 * 执行节点
 */
public class ShowInputDialogJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String title = parseStringField("title", input);
        String message = parseStringField("message", input);
        String keyField = parseStringField("keyField", input);
        if (StrUtil.isBlank(keyField)) {
            keyField = "key";
        }

        String value = UserIdeManager.getUserIdeInterface().showInputDialog(title, message);

        output.set(keyField, value);

    }
}
