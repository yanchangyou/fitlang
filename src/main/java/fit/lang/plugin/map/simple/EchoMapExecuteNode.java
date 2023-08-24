package fit.lang.plugin.map.simple;

import fit.lang.plugin.map.define.MapExecuteNode;
import fit.lang.plugin.map.define.MapExecuteNodeInput;
import fit.lang.plugin.map.define.MapExecuteNodeOutput;

/**
 * 执行节点
 */
public class EchoMapExecuteNode extends MapExecuteNode {

    @Override
    public void execute(MapExecuteNodeInput input, MapExecuteNodeOutput output) {
        output.getNodeData().getData().putAll(input.getNodeData().getData());
    }

}
