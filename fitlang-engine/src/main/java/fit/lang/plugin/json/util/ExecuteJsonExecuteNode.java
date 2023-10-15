package fit.lang.plugin.json.util;

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

        String result = ExecuteJsonNodeUtil.executeCode(nodeDefine, new JSONObject(), input.getNodeContext());

        output.setData(JSONObject.parseObject(result));

    }
}
