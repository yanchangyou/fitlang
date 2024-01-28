package fit.lang.common.flow;

import cn.hutool.core.util.StrUtil;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.ExecuteNode;
import fit.lang.define.ExecuteNodeInput;
import fit.lang.define.ExecuteNodeOutput;

import java.util.ArrayList;
import java.util.List;

/**
 * 执行节点
 */
public class SequenceExecuteNode extends AbstractExecuteNode {

    /**
     * 是否袋子模式，是：执行结果放入袋子中，否：最后的结果
     */
    protected boolean isBagsMode;

    protected String bagsName = "list";

    public boolean isBagsMode() {
        return isBagsMode;
    }

    public void setBagsMode(boolean bagsMode) {
        isBagsMode = bagsMode;
    }

    public String getBagsName() {
        return bagsName;
    }

    public void setBagsName(String bagsName) {
        if (StrUtil.isNotBlank(bagsName)) {
            this.bagsName = bagsName;
        }
    }

    public void setBags(String bagsFieldName, List list, ExecuteNodeOutput output) {
        //ignore
    }


    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {
        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        List<Object> bags = isBagsMode ? new ArrayList<>(childNodes.size()) : null;

        for (ExecuteNode childNode : childNodes) {
            if (this.isNeedCloneInputData()) {
                input.getNodeData().setData(input.getNodeData().cloneData());
            }
            childNode.executeAndNext(input, output);
            if (isBagsMode) {
                bags.add(output.getNodeData().cloneData());
            }
        }
        if (isBagsMode) {
            setBags(getBagsName(), bags, output);
        }

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }
}
