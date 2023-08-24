package fit.lang.plugin.any.define;

import fit.lang.define.base.ExecuteNodeData;

/**
 * 执行节点
 */
public class AnyTypeExecuteNodeData<T> implements ExecuteNodeData {

    public AnyTypeExecuteNodeData(T data) {
        setData(data);
    }

    T data;

    public T getData() {
        return data;
    }

    public void setData(Object data) {
        this.data = (T) data;
    }

    /**
     * TODO 未实现
     *
     * @return
     */
    @Override
    public Object cloneData() {
        return data;
    }
}
