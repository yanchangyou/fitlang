package fit.lang.plugin.map.define;

import fit.lang.define.base.ExecuteNodeData;

import java.util.HashMap;
import java.util.Map;

/**
 * 执行节点入参
 */
public class MapExecuteNodeData implements ExecuteNodeData {

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
        Map<String, Object> cloneMap = new HashMap<>();
        cloneMap.putAll(data);
        return cloneMap;
    }

    public void setData(Map<String, Object> data) {
        this.data = data;
    }

}
