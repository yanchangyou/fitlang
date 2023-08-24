package fit.lang.plugin.json.util;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeBuildable;

/**
 * 执行节点
 */
public class AddJsonExecuteNode extends JsonExecuteNode implements ExecuteNodeBuildable {

    String addField = "list";

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        JSONArray list = input.getJsonArray(addField);
        if (list == null) {
            throw new ExecuteNodeException("AddJsonExecuteNode addField is null!");
        }
        Object result = null;
        Integer sumInt = 0;
        double sum = 0;
        StringBuilder builder = new StringBuilder();
        JSONArray array = new JSONArray();
        JSONObject object = new JSONObject();
        for (Object item : list) {
            if (item instanceof Integer) {
                result = sumInt += (Integer) item;
            } else if (item instanceof Number) {
                result = sum += ((Number) item).doubleValue();
            } else if (item instanceof String) {
                builder.append(item);
            } else if (item instanceof JSONArray) {
                array.addAll((JSONArray) item);
                result = array;
            } else if (item instanceof JSONObject) {
                object.putAll((JSONObject) item);
                result = object;
            }
        }
        if (builder.length() > 0) {
            result = builder.toString();
        }

        output.set("result", result);
    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        String configAddFieldName = ((JsonExecuteNodeData) executeNodeData).getData().getString("addField");
        if (configAddFieldName != null) {
            addField = configAddFieldName;
        }
    }
}
