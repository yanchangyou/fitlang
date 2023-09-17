package fit.lang.plugin.map.define;

import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeInput;

/**
 * 执行节点入参
 */
public class MapExecuteNodeInput implements ExecuteNodeInput {

    MapExecuteNodeData nodeData;

    MapExecuteContext nodeContext;

    public MapExecuteNodeInput(MapExecuteContext nodeContext) {
        this(new MapExecuteNodeData(), nodeContext);
    }

    public MapExecuteNodeInput(MapExecuteNodeData nodeData, MapExecuteContext nodeContext) {
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

    @Override
    public ExecuteNodeInput createInput() {
        return new MapExecuteNodeInput(this.nodeContext);
    }

    public void setNodeData(MapExecuteNodeData nodeData) {
        this.nodeData = nodeData;
    }
}
