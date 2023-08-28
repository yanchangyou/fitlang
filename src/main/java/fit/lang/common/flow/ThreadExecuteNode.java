package fit.lang.common.flow;

import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;

/**
 * 执行节点
 */
public class ThreadExecuteNode extends AbstractExecuteNode {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {
        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        new Thread() {
            @Override
            public void run() {
                for (ExecuteNode childNode : childNodes) {
                    if (isNeedCloneInputData()) {
                        input.getNodeData().setData(input.getNodeData().cloneData());
                    }
                    childNode.executeAndNext(input, output);
                }
            }
        }.start();

        output.setNodeData(input.getNodeData());

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }

}
