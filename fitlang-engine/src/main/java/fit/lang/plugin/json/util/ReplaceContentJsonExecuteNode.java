package fit.lang.plugin.json.util;

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

        String content = parseStringField("content", input);

        String result = content.replaceAll(find, replace);

        output.set("content", result);

    }
}
