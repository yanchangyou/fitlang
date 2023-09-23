package fit.lang.common.flow;

import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;

import java.util.List;

/**
 * 执行节点
 */
public abstract class LoopExecuteNode extends AbstractExecuteNode {

    int loopTimes = 1;

    int currentIndex = 0;

    /**
     * loop参数处理逻辑，是否pipe模式（出参转下一次入参），默认是否
     */
    boolean isPipe;

    /**
     * 是否袋子模式，是：执行结果放入袋子中，否：最后的结果
     */
    boolean isBagsMode;

    String bagsName = "list";

    /**
     * 获取循环次数
     *
     * @return
     */
    public int getLoopTimes() {
        return loopTimes;
    }

    public void setLoopTimes(Integer loopTimes) {
        if (loopTimes != null) {
            this.loopTimes = loopTimes;
        }
    }

    public void setLoopTimes(int loopTimes) {
        this.loopTimes = loopTimes;
    }

    public int getCurrentIndex() {
        return currentIndex;
    }

    public void setCurrentIndex(int currentIndex) {
        this.currentIndex = currentIndex;
    }

    public boolean isPipe() {
        return isPipe;
    }

    public void setPipe(boolean pipe) {
        isPipe = pipe;
    }

    public void setPipe(Boolean pipe) {
        isPipe = Boolean.TRUE.equals(pipe);
    }

    public boolean isBagsMode() {
        return isBagsMode;
    }

    public void setBagsMode(boolean bagsMode) {
        this.isBagsMode = bagsMode;
    }

    public String getBagsName() {
        return bagsName;
    }

    public void setBagsName(String bagsName) {
        if (bagsName != null) {
            this.bagsName = bagsName;
        }
    }

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);
        currentIndex = 0;
        List bags = getBags(getLoopTimes());
        for (int i = 0; i < getLoopTimes(); i++) {
            input.getNodeContext().setAttribute("loopIndex", currentIndex);
            for (ExecuteNode executeNode : childNodes) {
                executeNode.executeAndNext(input, output);
                if (isPipe) {
                    input.setNodeData(output.getNodeData());
                }
            }
            if (isBagsMode) {
                bags.add(output.getNodeData().getData());
            }
            currentIndex++;
        }

        if (isBagsMode) {
            setBags(bagsName, bags, output);
        }
        ExecuteNodeSimpleAop.afterExecute(input, this, output);
    }

    public abstract List getBags(int size);

    public abstract void setBags(String bagsFieldName, List list, ExecuteNodeOutput output);

}
