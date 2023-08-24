package fit.lang.plugin.json.define;

import fit.lang.define.base.ExecuteNodeInput;

/**
 * 执行节点入参
 */
public class JsonExecuteNodeInput extends JsonExecuteNodePut implements ExecuteNodeInput {

    public JsonExecuteNodeInput(JsonExecuteContext nodeContext) {
        this(new JsonExecuteNodeData(), nodeContext);
    }

    public JsonExecuteNodeInput(JsonExecuteNodeData data, JsonExecuteContext nodeContext) {
        super(data, nodeContext);
    }
}