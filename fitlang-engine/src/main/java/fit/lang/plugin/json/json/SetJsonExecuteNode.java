package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONPath;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class SetJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String path = nodeJsonDefine.getString("path");

        Object value = parseField("value", input);

        JSONObject inputData = input.getData();

        JSONPath.set(inputData, path, value);

        output.setData(inputData);
    }
}
