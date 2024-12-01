package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.define.ExecuteNodeBuildable;
import fit.lang.define.ExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

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
        int sumInt = 0;
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
        if (!builder.isEmpty()) {
            result = builder.toString();
        }

        output.set("result", result);
    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        String configAddFieldName = getJsonData(executeNodeData).getString("addField");
        if (configAddFieldName != null) {
            addField = configAddFieldName;
        }
    }
}
