package fit.lang.plugin.json.ide;

import cn.hutool.core.util.StrUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

/**
 * 执行节点
 */
public class WriteEditorJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String contentField = parseStringField("contentField", input);

        String content;

        if (StrUtil.isBlank(contentField)) {
            content = toJsonTextWithFormat(input.getData());
        } else {
            content = input.getString(contentField);
        }

        UserIdeManager.getUserIdeInterface().writeEditorContent(content);
        output.setData(input.getData());

    }
}
