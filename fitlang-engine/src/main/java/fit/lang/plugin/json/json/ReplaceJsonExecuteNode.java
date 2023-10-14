package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;

/**
 * 执行节点
 */
public class ReplaceJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        Object replaceJson = nodeJsonDefine.get("json");
        if (replaceJson == null) {
            replaceJson = new JSONObject();
        }
        Object replaceJsonResult = ExpressUtil.eval(replaceJson, input.getInputParamAndContextParam());
        JSONObject result;
        if (replaceJsonResult instanceof JSONObject) {
            result = (JSONObject) replaceJsonResult;
        } else if (isJsonObjectText(replaceJsonResult)) {
            result = JSONObject.parse((String) replaceJsonResult);
        } else {
            throw new ExecuteNodeException("replaceJson node not supported json field value: " + replaceJson);
        }
        output.setData(result);
    }
}
