package fit.lang.plugin.object.define;

import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeData;

/**
 * 执行节点入参
 */
public class ObjectExecuteNodeInput implements ExecuteNodeInput {
    ObjectExecuteNodeData data;
    ObjectExecuteContext nodeContext;

    public ObjectExecuteNodeInput(ObjectExecuteContext nodeContext) {
        this.nodeContext = nodeContext;
    }

    @Override
    public ObjectExecuteContext getNodeContext() {
        return nodeContext;
    }

    @Override
    public ObjectExecuteNodeData getNodeData() {
        return data;
    }

    public void setData(ObjectExecuteNodeData data) {
        this.data = data;
    }

    @Override
    public void setNodeData(ExecuteNodeData executeNodeData) {
        this.data = (ObjectExecuteNodeData) executeNodeData;
    }

    @Override
    public ExecuteNodeInput createInput() {
        return new ObjectExecuteNodeInput(this.nodeContext);
    }
}
