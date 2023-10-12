package fit.lang.plugin.json.define;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.base.ExecuteNodeData;

/**
 * 执行节点入参
 */
public class JsonExecuteNodeData implements ExecuteNodeData {

    JSONObject data;

    JsonExecuteNodeData() {
        data = new JSONObject();
    }

    public JsonExecuteNodeData(JSONObject item) {
        data = item;
    }

    public JSONObject getData() {
        return data;
    }

    @Override
    public void setData(Object data) {
        setData((JSONObject) data);
    }

    @Override
    public Object cloneData() {
        return data.clone();
    }

    @Override
    public ExecuteNodeData cloneThis() {
        return new JsonExecuteNodeData((JSONObject) cloneData());
    }

    public void setData(JSONObject data) {
        this.data = data;
    }

    public boolean isEmpty() {
        return data == null || data.isEmpty();
    }

    public boolean containsKey(String fieldName) {
        return data.containsKey(fieldName);
    }

    public void set(String fieldName, Object fieldValue) {
        data.put(fieldName, fieldValue);
    }

    public Object get(String fieldName) {
        return data.get(fieldName);
    }

    public Object getObject(String fieldName) {
        return get(fieldName);
    }

    public String getString(String fieldName) {
        return data.getString(fieldName);
    }

    public Integer getInteger(String fieldName) {
        return data.getInteger(fieldName);
    }

    public Double getDouble(String fieldName) {
        return data.getDouble(fieldName);
    }

    public JSONObject getJsonObject(String fieldName) {
        return data.getJSONObject(fieldName);
    }

    public JSONArray getJsonArray(String fieldName) {
        return data.getJSONArray(fieldName);
    }

    public Object remove(String fieldName) {
        return data.remove(fieldName);
    }

}
