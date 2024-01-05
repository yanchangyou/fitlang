package fit.lang.plugin.json.os;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

/**
 * 执行节点
 */
public class SetClipboardJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String contentField = parseStringField("contentField", input);

        Object content;
        if (StrUtil.isBlank(contentField)) {
            content = input.getData();
        } else {
            content = input.get(contentField);
        }

        String text;
        Boolean format = nodeJsonDefine.getBoolean("format");
        if (Boolean.TRUE.equals(format) && content instanceof JSONObject) {
            text = toJsonTextWithFormat((JSONObject) content);
        } else {
            text = content.toString();
        }

        Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        StringSelection selection = new StringSelection(text);
        clipboard.setContents(selection, null);

        output.setData(input.getData());

    }
}
