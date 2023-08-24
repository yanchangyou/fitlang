package fit.lang.plugin.object.define;

import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeOutput;

/**
 * 执行节点出参
 */
public class ObjectExecuteNodeOutput implements ExecuteNodeOutput {

    ObjectExecuteNodeData data;

    ObjectExecuteContext nodeContext;

    public ObjectExecuteNodeOutput(ObjectExecuteContext nodeContext) {
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
        setData((ObjectExecuteNodeData) executeNodeData);
    }
}
