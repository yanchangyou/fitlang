package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteReturnNodeException;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonText;

/**
 * 执行节点
 */
public class AssertJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        Boolean needToString = nodeJsonDefine.getBoolean("needToString");

        boolean success = true;
        Object assertResultObject = null;

        String type = "";
        if (nodeJsonDefine.get("expected") != null) {
            type = "expected";
            Object expectedExpress = nodeJsonDefine.get("expected");
            assertResultObject = ExpressUtil.eval(expectedExpress, input.getInputParamAndContextParam());

            if (Boolean.TRUE.equals(needToString) && assertResultObject != null) {
                success = input.getData().toJSONString().equals(assertResultObject.toString());
            } else {
                success = input.getData().equals(assertResultObject);
            }
        }
        if (success && nodeJsonDefine.get("containField") != null) {
            type = "containField";
            JSONArray containField = nodeJsonDefine.getJSONArray("containField");
            assertResultObject = "contain fields: ".concat(ExpressUtil.eval(containField, input.getInputParamAndContextParam()).toString());

            for (Object field : containField) {
                if (!input.containsKey(field.toString())) {
                    success = false;
                    break;
                }
            }
        }
        if (success && nodeJsonDefine.get("containJson") != null) {
            type = "containJson";
            JSONObject containJson = nodeJsonDefine.getJSONObject("containJson");
            assertResultObject = toJsonText(ExpressUtil.eval(containJson, input.getInputParamAndContextParam()));
            for (String field : containJson.keySet()) {
                if (!containJson.get(field).equals(input.get(field))) {
                    success = false;
                    break;
                }
            }
        }

        JSONObject result = new JSONObject();
        result.put("success", success);
        if (!success) {
            result.put("type", type);
            if (Boolean.TRUE.equals(needToString)) {
                result.put("actual", input.getData().toString());
            } else {
                result.put("actual", input.getData());
            }
            result.put("expected", assertResultObject);
            result.put("input", input.getData());

            //断言失败返回
            throw new ExecuteReturnNodeException(result);
        }
        //断言成功透传
        output.setData(input.getData());

    }
}
