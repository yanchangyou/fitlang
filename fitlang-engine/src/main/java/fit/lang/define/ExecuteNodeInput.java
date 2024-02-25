package fit.lang.define;

/**
 * 执行节点入参
 */
public interface ExecuteNodeInput {

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

    /**
     * 创建入参
     * @return
     */
    ExecuteNodeInput createInput();

}
