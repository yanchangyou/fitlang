package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ReplaceJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        JSONObject replaceJson = nodeJsonDefine.getJSONObject("json");
        if (replaceJson == null) {
            replaceJson = new JSONObject();
        }
        JSONObject replaceJsonResult = ExpressUtil.eval(replaceJson, input.getInputParamAndContextParam());
        output.setData(replaceJsonResult);
    }
}
