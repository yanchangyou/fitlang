package fit.lang.plugin.object.simple;

import fit.lang.plugin.object.define.ObjectExecuteNode;
import fit.lang.plugin.object.define.ObjectExecuteNodeInput;
import fit.lang.plugin.object.define.ObjectExecuteNodeOutput;

/**
 * 执行节点
 */
public class EchoObjectExecuteNode extends ObjectExecuteNode {

    @Override
    public void execute(ObjectExecuteNodeInput input, ObjectExecuteNodeOutput output) {
        output.setNodeData(input.getNodeData());
    }

}
