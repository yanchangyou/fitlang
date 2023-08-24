package fit.lang.plugin.json.define;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import fit.lang.define.base.ExecuteNodeData;

/**
 * 执行节点入参或出参
 */
public class JsonExecuteNodePut {

    JsonExecuteContext nodeContext;

    JsonExecuteNodeData data;

    public JsonExecuteContext getNodeContext() {
        return nodeContext;
    }

    public void setNodeContext(JsonExecuteContext nodeContext) {
        this.nodeContext = nodeContext;
    }

    public JsonExecuteNodePut(JsonExecuteContext nodeContext) {
        this(new JsonExecuteNodeData(), nodeContext);
    }

    public JsonExecuteNodePut(JsonExecuteNodeData data, JsonExecuteContext nodeContext) {
        setNodeContext(nodeContext);
        setNodeData(data);
    }

    public JsonExecuteNodeData getNodeData() {
        return data;
    }

    public void setNodeData(ExecuteNodeData executeNodeData) {
        setNodeData((JsonExecuteNodeData) executeNodeData);
    }

    public void setNodeData(JsonExecuteNodeData data) {
        this.data = data;
    }

    public boolean isEmpty() {
        return data == null || data.isEmpty();
    }

    public boolean containsKey(String fieldName) {
        return data.containsKey(fieldName);
    }

    public void set(String fieldName, Object fieldValue) {
        data.set(fieldName, fieldValue);
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
        return data.getJsonObject(fieldName);
    }

    public JSONArray getJsonArray(String fieldName) {
        return data.getJsonArray(fieldName);
    }

    public Object remove(String fieldName) {
        return data.remove(fieldName);
    }

    public JSONObject getData() {
        return data.getData();
    }

    public void setData(JSONObject data) {
        this.data.setData(data);
    }
}
