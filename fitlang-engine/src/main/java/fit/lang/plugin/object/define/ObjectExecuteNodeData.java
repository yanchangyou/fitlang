package fit.lang.plugin.object.define;

import fit.lang.define.ExecuteNodeData;

/**
 * 执行节点
 */
public class ObjectExecuteNodeData implements ExecuteNodeData {

    public ObjectExecuteNodeData(Object data) {
        setData(data);
    }

    Object data;

    public Object getData() {
        return data;
    }

    public void setData(Object data) {
        this.data = data;
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

    @Override
    public ExecuteNodeData cloneThis() {
        return new ObjectExecuteNodeData(cloneData());
    }

}
