package fit.lang.plugin.any.simple;

import fit.lang.plugin.any.define.AnyTypeExecuteNode;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeInput;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeOutput;

/**
 * 执行节点
 */
public class EchoAnyTypeExecuteNode extends AnyTypeExecuteNode {

    @Override
    public void execute(AnyTypeExecuteNodeInput input, AnyTypeExecuteNodeOutput output) {
        output.setNodeData(input.getNodeData());
    }

}
