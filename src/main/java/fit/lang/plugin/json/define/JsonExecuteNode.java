package fit.lang.plugin.json.define;

import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;

/**
 * 执行节点
 */
public abstract class JsonExecuteNode extends AbstractExecuteNode {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        execute((JsonExecuteNodeInput) input, (JsonExecuteNodeOutput) output);

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }

    public abstract void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output);

}
