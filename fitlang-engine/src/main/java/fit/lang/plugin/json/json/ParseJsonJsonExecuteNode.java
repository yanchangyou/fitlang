package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonArrayText;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;

/**
 * 执行节点
 */
public class ParseJsonJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String jsonField = parseStringField("jsonField", input);
        if (jsonField == null) {
            throw new ExecuteNodeException("parseJson jsonField is required!");
        }
        Object value = input.get(jsonField);

        JSONObject outputJson = input.getData().clone();
        output.setData(outputJson);

        if (value instanceof String) {
            if (isJsonObjectText(value)) {
                value = JSONObject.parse((String) value);
            } else if (isJsonArrayText(value)) {
                value = JSONArray.parse((String) value);
            }

            output.set(jsonField, value);

        }
    }
}
