package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class EvalJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject evalJson = nodeJsonDefine.getJSONObject("json");
        if (evalJson == null) {
            evalJson = input.getData();
        }
        JSONObject evalResult = ExpressUtil.eval(evalJson, input.getInputParamAndContextParam());
        output.setData(evalResult);
    }
}
