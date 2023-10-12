package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ParseJsonJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String jsonField = parseStringField("jsonField", input);
        String jsonText = input.getString(jsonField);

        JSONObject outputJson = input.getData().clone();

        outputJson.put(jsonField, JSONObject.parseObject(jsonText));

        output.setData(outputJson);
    }
}
