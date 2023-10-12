package fit.lang.plugin.json.util;

import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class RemoveEmptyFieldJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        for (String field : input.getNodeData().getData().keySet()) {
            if (input.get(field) == null || "".equals(input.get(field))) {
                continue;
            }
            output.set(field, input.get(field));
        }
    }
}
