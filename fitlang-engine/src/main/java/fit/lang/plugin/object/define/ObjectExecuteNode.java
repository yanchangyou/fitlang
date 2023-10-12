package fit.lang.plugin.object.define;

import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;

/**
 * 执行节点
 */
public abstract class ObjectExecuteNode extends AbstractExecuteNode {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        execute((ObjectExecuteNodeInput) input, (ObjectExecuteNodeOutput) output);

        ExecuteNodeSimpleAop.afterExecute(input, this, output);
    }

    public abstract void execute(ObjectExecuteNodeInput input, ObjectExecuteNodeOutput output);

}
