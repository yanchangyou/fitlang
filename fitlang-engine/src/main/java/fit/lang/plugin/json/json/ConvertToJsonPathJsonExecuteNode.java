package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 转换到json path的平铺结构
 */
public class ConvertToJsonPathJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject inputJson = input.getData();

        JSONObject outputJson = ExecuteJsonNodeUtil.convertToJsonPath(inputJson);

        output.setData(outputJson);
    }
}
