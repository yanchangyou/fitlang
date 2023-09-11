package fit.lang.plugin.json.util;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class MixJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        //挑选一个字段mix，此字段值必须是jsonObject
        String mixField = nodeJsonDefine.getString("mixField");

        JSONObject mixJson = nodeJsonDefine.getJSONObject("json");
        if (mixJson == null) {
            mixJson = new JSONObject();
        }
        JSONObject mixJsonResult = ExpressUtil.eval(mixJson, input.getInputParamAndContextParam());
        JSONObject outputJson = input.getData().clone();
        if (StrUtil.isBlank(mixField)) {
            outputJson.putAll(mixJsonResult);
        } else if (mixJsonResult != null) {
            Object mixValue = mixJsonResult.get(mixField);
            if (mixValue instanceof JSONObject) {
                outputJson.putAll((JSONObject) mixValue);
            }
        }
        output.setData(outputJson);
    }
}
