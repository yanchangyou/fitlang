package fit.lang.plugin.any.define;

import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeInput;

/**
 * 执行节点入参
 */
public class AnyTypeExecuteNodeInput implements ExecuteNodeInput {
    AnyTypeExecuteNodeData data;
    AnyTypeExecuteContext nodeContext;

    public AnyTypeExecuteNodeInput(AnyTypeExecuteContext nodeContext) {
        this.nodeContext =nodeContext;
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
        this.data = (AnyTypeExecuteNodeData) executeNodeData;
    }
}
