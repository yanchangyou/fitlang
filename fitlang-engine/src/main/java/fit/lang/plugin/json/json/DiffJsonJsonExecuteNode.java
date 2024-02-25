package fit.lang.plugin.json.json;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 比较两个json,只显示差异部分
 */
public class DiffJsonJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject inputJson = input.getData();

        String json1Field = parseStringField("json1Field", input);
        String json2Field = parseStringField("json2Field", input);

        if (StrUtil.isBlank(json1Field)) {
            json1Field = "json1";
        }

        if (StrUtil.isBlank(json2Field)) {
            json2Field = "json2";
        }

        JSONObject json1 = inputJson.getJSONObject(json1Field);
        JSONObject json2 = inputJson.getJSONObject(json2Field);

        JSONObject outputJson = ExecuteJsonNodeUtil.diffJsonObject(json1, json2);

        output.setData(outputJson);
    }
}
