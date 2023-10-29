package fit.lang.plugin.json.json;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

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

        String outputField = jsonField;
        if (StrUtil.isBlank(jsonField)) {
            outputField = "json";
        }
        String content = null;
        if (jsonObject != null) {
            if (Boolean.TRUE.equals(needFormat)) {
                content = toJsonTextWithFormat(jsonObject);
            } else {
                content = jsonObject.toJSONString(JSONWriter.Feature.WriteMapNullValue);
            }
        }
        output.set(outputField, content);
    }
}
