package fit.lang.plugin.json.json;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.CompareUtils;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 比较两个json
 */
public class CompareJsonJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject inputJson = input.getData();

        boolean onlyDiff = Boolean.TRUE.equals(nodeJsonDefine.getBoolean("onlyDiff"));

        boolean toArray = Boolean.TRUE.equals(nodeJsonDefine.getBoolean("toArray"));

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

        Object result;
        if (toArray) {
            if (onlyDiff) {
                result = CompareUtils.diffToArray(json1, json2);
            } else {
                result = CompareUtils.compareToArray(json1, json2);
            }
        } else {
            if (onlyDiff) {
                result = CompareUtils.diff(json1, json2);

            } else {
                result = CompareUtils.compare(json1, json2);
            }
        }
        JSONArray list = CompareUtils.compareToArray(json1, json2);
        JSONObject outputJson = CompareUtils.sum(list);

        outputJson.put("result", result);

        output.setData(outputJson);
    }
}
