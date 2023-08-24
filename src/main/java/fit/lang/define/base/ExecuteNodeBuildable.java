package fit.lang.define.base;

/**
 * 执行节点: 需要自身节点
 */
public interface ExecuteNodeBuildable {

    /**
     * 构建
     *
     * @param executeNodeData
     */
    void build(ExecuteNodeData executeNodeData);

}
