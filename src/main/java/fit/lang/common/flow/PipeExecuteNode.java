package fit.lang.common.flow;

import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;

/**
 * 执行节点
 */
public class PipeExecuteNode extends AbstractExecuteNode {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        for (int i = 0; i < childNodes.size(); i++) {
            ExecuteNode childNode = childNodes.get(i);
            if (i > 0) {
                input.setNodeData(output.getNodeData());
            }
            childNode.executeAndNext(input, output);
        }

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }
}
