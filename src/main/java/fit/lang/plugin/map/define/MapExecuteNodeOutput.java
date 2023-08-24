package fit.lang.plugin.map.define;

import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeOutput;

/**
 * 执行节点出参
 */
public class MapExecuteNodeOutput implements ExecuteNodeOutput {

    MapExecuteNodeData nodeData;

    MapExecuteContext nodeContext;

    public MapExecuteNodeOutput(MapExecuteContext nodeContext) {
        this(new MapExecuteNodeData(), nodeContext);
    }

    public MapExecuteNodeOutput(MapExecuteNodeData nodeData, MapExecuteContext nodeContext) {
        setNodeData(nodeData);
        this.nodeContext = nodeContext;
    }

    @Override
    public MapExecuteContext getNodeContext() {
        return nodeContext;
    }

    @Override
    public MapExecuteNodeData getNodeData() {
        return nodeData;
    }

    @Override
    public void setNodeData(ExecuteNodeData executeNodeData) {
        setNodeData((MapExecuteNodeData) executeNodeData);
    }

    public void setNodeData(MapExecuteNodeData nodeData) {
        this.nodeData = nodeData;
    }
}
