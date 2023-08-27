package fit.lang.plugin.json.util;

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
        JSONObject mixJson = nodeJsonDefine.getJSONObject("json");
        if (mixJson == null) {
            mixJson = new JSONObject();
        }
        JSONObject mixJsonResult = ExpressUtil.eval(mixJson, input.getInputParamAndContextParam());
        JSONObject outputJson = input.getData().clone();
        outputJson.putAll(mixJsonResult);
        output.setData(outputJson);
    }
}
