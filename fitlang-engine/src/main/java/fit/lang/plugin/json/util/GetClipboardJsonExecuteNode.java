package fit.lang.plugin.json.util;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.awt.*;
import java.awt.datatransfer.*;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;


/**
 * 执行节点
 */
public class GetClipboardJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String contentField = parseStringField("contentField", input);


        Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        Transferable contentTransferable = clipboard.getContents(null);
        if (contentTransferable.isDataFlavorSupported(DataFlavor.stringFlavor)) {
            try {
                String text = (String) contentTransferable.getTransferData(DataFlavor.stringFlavor);

                if (isJsonObjectText(text)) {
                    output.setData(JSONObject.parse(text));
                } else {
                    if (StrUtil.isBlank(contentField)) {
                        contentField = "content";
                    }
                    output.set(contentField, text);
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }

    }
}
