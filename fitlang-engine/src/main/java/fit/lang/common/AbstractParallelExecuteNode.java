package fit.lang.common;

/**
 * 执行节点
 */
public abstract class AbstractParallelExecuteNode extends AbstractExecuteNode {

    /**
     * 并行数
     */
    protected int parallelism = 1;

    public int getParallelism() {
        return parallelism;
    }

    public void setParallelism(int parallelism) {
        this.parallelism = parallelism;
    }

    public void setParallelism(Integer parallelism) {
        if (parallelism != null) {
            this.parallelism = parallelism;
        }
    }
}
