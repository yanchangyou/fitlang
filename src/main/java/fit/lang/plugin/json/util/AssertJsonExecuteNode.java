package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class AssertJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        Boolean needToString = nodeJsonDefine.getBoolean("needToString");

        Object expectedExpress = nodeJsonDefine.get("expected");
        if (expectedExpress == null) {
            throw new ExecuteNodeException("assert expected field is empty!");
        }
        Object evalResult = ExpressUtil.eval(expectedExpress, input.getInputParamAndContextParam());

        boolean success;
        if (Boolean.TRUE.equals(needToString) && evalResult != null) {
            success = input.getData().toJSONString().equals(evalResult.toString());
        } else {
            success = input.getData().equals(evalResult);
        }

        JSONObject result = new JSONObject();
        result.put("success", success);
        if (!success) {
            if (Boolean.TRUE.equals(needToString)) {
                result.put("actual", input.getData().toString());
            } else {
                result.put("actual", input.getData());
            }
            result.put("expected", evalResult);
            result.put("input", input.getData());
        }
        output.setData(result);
    }
}
