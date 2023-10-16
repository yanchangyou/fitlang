package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

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
        Object jsonObject = input.get(jsonField);

        JSONObject outputJson = input.getData().clone();
        output.setData(outputJson);

        if (jsonObject instanceof String) {
            if (isJsonObjectText(jsonObject)) {
                output.set(jsonField, JSONObject.parseObject((String) jsonObject));
            }
        }
    }
}
