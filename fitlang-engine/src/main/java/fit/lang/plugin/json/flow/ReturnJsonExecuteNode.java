package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ReturnJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject returnJson = nodeJsonDefine.getJSONObject("json");
        if (returnJson == null) {
            returnJson = new JSONObject();
        }
        JSONObject returnJsonResult = ExpressUtil.eval(returnJson, input.getInputParamAndContextParam());
        output.setData(returnJsonResult);
    }
}
