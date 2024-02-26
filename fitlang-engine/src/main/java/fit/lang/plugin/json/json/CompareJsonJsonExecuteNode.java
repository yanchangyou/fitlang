package fit.lang.plugin.json.json;

import cn.hutool.core.util.StrUtil;
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

        JSONObject outputJson = new JSONObject();
        if (toArray) {
            if (onlyDiff) {
                outputJson.put("array", CompareUtils.diffToArray(json1, json2));
            } else {
                outputJson.put("array", CompareUtils.compareToArray(json1, json2));
            }
        } else {
            if (onlyDiff) {
                outputJson = CompareUtils.diff(json1, json2);
            } else {
                outputJson = CompareUtils.compare(json1, json2);
            }
        }

        output.setData(outputJson);
    }
}
