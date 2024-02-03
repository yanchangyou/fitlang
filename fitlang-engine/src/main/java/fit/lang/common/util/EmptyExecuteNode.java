package fit.lang.common.util;

import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.ExecuteNodeInput;
import fit.lang.define.ExecuteNodeOutput;

/**
 * 执行节点:空节点，无任何逻辑，类属于数学的0，用于语法完整
 */
public class EmptyExecuteNode extends AbstractExecuteNode {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

    }

}
