package fit.lang.common.flow;

import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;

/**
 * 执行节点
 */
public class SequenceExecuteNode extends AbstractExecuteNode {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {
        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        for (ExecuteNode childNode : childNodes) {
            if (this.isNeedCloneInputData()) {
                input.getNodeData().setData(input.getNodeData().cloneData());
            }
            childNode.executeAndNext(input, output);
        }

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }
}
