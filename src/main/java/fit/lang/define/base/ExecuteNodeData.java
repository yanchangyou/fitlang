package fit.lang.define.base;

/**
 * 执行节点数据
 */
public interface ExecuteNodeData {

    /**
     * 获取数据
     *
     * @return
     */
    Object getData();

    /**
     * 设置数据
     *
     * @param data
     */
    void setData(Object data);

    /**
     * clone 数据
     *
     * @return
     */
    Object cloneData();

}
