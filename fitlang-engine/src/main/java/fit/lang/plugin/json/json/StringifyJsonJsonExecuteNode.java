package fit.lang.plugin.json.json;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class StringifyJsonJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String jsonField = parseStringField("jsonField", input);
        JSONObject jsonObject = input.getData();

        if (StrUtil.isNotBlank(jsonField)) {
            jsonObject = input.getJsonObject(jsonField);
        }

        JSONObject outputJson = new JSONObject();

        String outputField = jsonField;
        if (StrUtil.isBlank(jsonField)) {
            outputField = "json";
        }
        outputJson.put(outputField, jsonObject.toJSONString());

        output.setData(outputJson);
    }
}
