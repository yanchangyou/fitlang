package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class IncreaseJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String fieldName = nodeJsonDefine.getString("field");
        if (fieldName == null) {
            fieldName = "index";
        }

        Integer number = input.getInteger(fieldName);
        if (number == null) {
            number = 0;
        }
        number += 1;

        JSONObject result = input.getData().clone();
        result.put(fieldName, number);
        output.setData(result);
    }
}
