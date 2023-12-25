package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONPath;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getWildlyField;

/**
 * 执行节点
 */
public class SetJsonExecuteNode extends JsonExecuteNode {

    static String[] WILDLY_FIELDS = new String[]{"path", "field", "key"};

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String path = getWildlyField(nodeJsonDefine, WILDLY_FIELDS);

        Object value = parseField("value", input);

        JSONObject inputData = input.getData();

        JSONPath.set(inputData, path, value);

        output.setData(inputData);
    }
}
