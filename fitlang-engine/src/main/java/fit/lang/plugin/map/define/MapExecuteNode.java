package fit.lang.plugin.map.define;

import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.ExecuteNodeInput;
import fit.lang.define.ExecuteNodeOutput;

/**
 * 执行节点
 */
public abstract class MapExecuteNode extends AbstractExecuteNode {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        execute((MapExecuteNodeInput) input, (MapExecuteNodeOutput) output);

        ExecuteNodeSimpleAop.afterExecute(input, this, output);
    }

    public abstract void execute(MapExecuteNodeInput input, MapExecuteNodeOutput output);

}
