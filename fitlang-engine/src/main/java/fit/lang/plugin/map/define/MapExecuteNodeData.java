package fit.lang.plugin.map.define;

import fit.lang.define.ExecuteNodeData;

import java.util.HashMap;
import java.util.Map;

/**
 * 执行节点入参
 */
public class MapExecuteNodeData implements ExecuteNodeData {

    public MapExecuteNodeData() {
    }

    public MapExecuteNodeData(Map<String, Object> data) {
        this.data = data;
    }

    Map<String, Object> data = new HashMap<>();

    public Map<String, Object> getData() {
        return data;
    }

    @Override
    public void setData(Object data) {
        setData((Map<String, Object>) data);
    }

    @Override
    public Object cloneData() {
        return new HashMap<>(data);
    }

    @Override
    public ExecuteNodeData cloneThis() {
        return new MapExecuteNodeData((Map<String, Object>) cloneData());
    }

    public void setData(Map<String, Object> data) {
        this.data = data;
    }

}
