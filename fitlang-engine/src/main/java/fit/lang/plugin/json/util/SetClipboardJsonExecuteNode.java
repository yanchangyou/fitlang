package fit.lang.plugin.json.util;

import cn.hutool.core.util.StrUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonText;

/**
 * 执行节点
 */
public class SetClipboardJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String contentField = parseStringField("contentField", input);

        String content = input.getString(contentField);
        if (StrUtil.isBlank(contentField)) {
            content = toJsonText(input.getData());
        }

        Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        StringSelection selection = new StringSelection(content);
        clipboard.setContents(selection, null);

        output.setData(input.getData());

    }
}
