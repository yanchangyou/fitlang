package fit.lang.plugin.any.define;

import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeOutput;

/**
 * 执行节点出参
 */
public class AnyTypeExecuteNodeOutput implements ExecuteNodeOutput {

    AnyTypeExecuteNodeData data;

    AnyTypeExecuteContext nodeContext;

    public AnyTypeExecuteNodeOutput(AnyTypeExecuteContext nodeContext) {
    }

    @Override
    public AnyTypeExecuteContext getNodeContext() {
        return nodeContext;
    }

    @Override
    public AnyTypeExecuteNodeData getNodeData() {
        return data;
    }

    public void setData(AnyTypeExecuteNodeData data) {
        this.data = data;
    }

    @Override
    public void setNodeData(ExecuteNodeData executeNodeData) {
        setData((AnyTypeExecuteNodeData) executeNodeData);
    }

    @Override
    public ExecuteNodeOutput createOutput() {
        return new AnyTypeExecuteNodeOutput(this.nodeContext);
    }
}
