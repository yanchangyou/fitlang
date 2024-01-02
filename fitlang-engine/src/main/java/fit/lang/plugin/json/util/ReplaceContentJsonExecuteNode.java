package fit.lang.plugin.json.util;

import cn.hutool.core.util.StrUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ReplaceContentJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String find = nodeJsonDefine.getString("find");
        String replace = nodeJsonDefine.getString("replace");
        boolean isRegexp = nodeJsonDefine.getBooleanValue("isRegexp", true);

        String contentField = parseStringField("contentField", input);

        if (StrUtil.isBlank(contentField)) {
            contentField = "content";
        }

        String content = parseStringField(contentField, input);

        String result;
        if (isRegexp) {
            result = content.replaceAll(find, replace);
        } else {
            result = content.replace(find, replace);
        }

        output.setData(input.getData().clone());
        output.set("content", result);

    }
}
