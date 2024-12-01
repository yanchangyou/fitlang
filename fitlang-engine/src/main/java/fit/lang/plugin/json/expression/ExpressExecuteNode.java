package fit.lang.plugin.json.expression;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.define.ExecuteNodeData;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public abstract class ExpressExecuteNode extends JsonExecuteNode implements ExecuteNodeEval {

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        JSONObject nodeDefineJson = getJsonData(executeNodeData);
        ExecuteNodeUtil.buildChildNode(this, nodeDefineJson);
    }

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
