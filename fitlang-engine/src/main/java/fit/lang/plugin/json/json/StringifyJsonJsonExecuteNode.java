package fit.lang.plugin.json.json;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonText;
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

        String outputField = jsonField;
        if (StrUtil.isBlank(jsonField)) {
            outputField = "json";
        }

        String content = null;

        Object value = jsonObject;
        if (StrUtil.isNotBlank(jsonField)) {
            value = input.get(jsonField);
        }

        if (value != null) {
            if (value instanceof JSONObject) {
                jsonObject = (JSONObject) value;
                if (Boolean.TRUE.equals(needFormat)) {
                    content = toJsonTextWithFormat(jsonObject);
                } else {
                    content = toJsonText(jsonObject);
                }
            } else if (value instanceof JSONArray) {
                JSONArray array = (JSONArray) value;
                if (Boolean.TRUE.equals(needFormat)) {
                    content = toJsonTextWithFormat(array);
                } else {
                    content = toJsonText(array);
                }
            } else {
                content = value.toString();
            }
        }
        output.set(outputField, content);
    }
}
