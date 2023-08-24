package fit.lang.common.flow;

import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;

/**
 * 执行节点
 */
public class LoopExecuteNode extends AbstractExecuteNode {

    int loopTimes = 1;

    int currentIndex = 0;

    /**
     * 获取循环次数
     *
     * @return
     */
    public int getLoopTimes() {
        return loopTimes;
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

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        for (int i = 0; i < getLoopTimes(); i++) {
            currentIndex++;
            for (ExecuteNode executeNode : childNodes) {
                executeNode.executeAndNext(input, output);
            }
        }

        ExecuteNodeSimpleAop.afterExecute(input, this, output);
    }

}
