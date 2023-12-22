package fit.lang.plugin.json.json;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class getJsonStructureExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        output.setData(ExecuteJsonNodeUtil.getJsonStructure(input.getData()));
    }

}
