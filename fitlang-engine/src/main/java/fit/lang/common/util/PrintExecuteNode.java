package fit.lang.common.util;

import com.alibaba.fastjson2.JSON;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;

/**
 * 执行节点
 */
public class PrintExecuteNode extends AbstractExecuteNode {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {
        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        System.out.println("input:" + JSON.toJSONString(input));
        System.out.println("output:" + JSON.toJSONString(output));

        output.setNodeData(input.getNodeData());

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }

}
