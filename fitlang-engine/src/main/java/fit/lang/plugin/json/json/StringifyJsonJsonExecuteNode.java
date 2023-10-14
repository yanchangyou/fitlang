package fit.lang.plugin.json.json;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
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
        Boolean needFormat = nodeJsonDefine.getBoolean("format");

        JSONObject jsonObject = input.getData();

        if (StrUtil.isNotBlank(jsonField)) {
            jsonObject = input.getJsonObject(jsonField);
        }

        JSONObject outputJson = new JSONObject();

        String outputField = jsonField;
        if (StrUtil.isBlank(jsonField)) {
            outputField = "json";
        }
        String content = null;
        if (jsonObject != null) {
            if (needFormat != null && needFormat) {
                content = jsonObject.toJSONString(JSONWriter.Feature.WriteMapNullValue, JSONWriter.Feature.PrettyFormat).replaceAll("\\t", "    ");
            } else {
                content = jsonObject.toJSONString(JSONWriter.Feature.WriteMapNullValue);
            }
        }
        outputJson.put(outputField, content);

        output.setData(outputJson);
    }
}
