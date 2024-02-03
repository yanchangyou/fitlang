package fit.lang.plugin.json.define;

import fit.lang.define.ExecuteNodeOutput;

/**
 * 执行节点出参
 */
public class JsonExecuteNodeOutput extends JsonExecuteNodePut implements ExecuteNodeOutput {

    public JsonExecuteNodeOutput(JsonExecuteContext nodeContext) {
        this(new JsonExecuteNodeData(), nodeContext);
    }

    public JsonExecuteNodeOutput(JsonExecuteNodeData data, JsonExecuteContext nodeContext) {
        super(data, nodeContext);
    }

    @Override
    public ExecuteNodeOutput createOutput() {
        return new JsonExecuteNodeOutput(this.nodeContext);
    }
}