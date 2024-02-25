package fit.lang.plugin.any.define;

import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.ExecuteNodeInput;
import fit.lang.define.ExecuteNodeOutput;

/**
 * 执行节点
 */
public abstract class AnyTypeExecuteNode extends AbstractExecuteNode {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        execute((AnyTypeExecuteNodeInput) input, (AnyTypeExecuteNodeOutput) output);

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

    }

    public abstract void execute(AnyTypeExecuteNodeInput input, AnyTypeExecuteNodeOutput output);

}
