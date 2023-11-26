package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ExecuteJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        JSONObject nodeDefine = input.getData();

        JSONObject inputJson = nodeDefine.getJSONObject("input");
        if (inputJson == null) {
            inputJson = new JSONObject();
        }

        String result = ExecuteJsonNodeUtil.executeCode(nodeDefine, inputJson, input.getNodeContext());

        output.setData(JSONObject.parseObject(result));

    }
}
