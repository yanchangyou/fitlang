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

        Object lastOutputData = null;
        for (int i = 0; i < childNodes.size(); i++) {
            ExecuteNode childNode = childNodes.get(i);
            ExecuteNodeInput itemInput;
            ExecuteNodeOutput itemOutput;
            if (i == 0) {
                itemInput = input;
            } else {
                itemInput = input.createInput();
                itemInput.getNodeData().setData(lastOutputData);
            }
            itemOutput = output.createOutput();
            childNode.executeAndNext(itemInput, itemOutput);
            lastOutputData = itemOutput.getNodeData().getData();
        }

        output.getNodeData().setData(lastOutputData);

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }
}
