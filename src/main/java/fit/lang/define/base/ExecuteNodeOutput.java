package fit.lang.define.base;

/**
 * 执行节点出参
 */
public interface ExecuteNodeOutput {

    /**
     * 上下文
     *
     * @return
     */
    ExecuteContext getNodeContext();

    /**
     * 数据
     *
     * @return
     */
    ExecuteNodeData getNodeData();

    void setNodeData(ExecuteNodeData executeNodeData);

}
