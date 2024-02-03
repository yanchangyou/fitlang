package fit.lang.plugin.json.json;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONPath;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ConvertToObjectArrayJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String arrayField = nodeJsonDefine.getString("arrayField");
        String keyField = nodeJsonDefine.getString("keyField");

        if (StrUtil.isBlank(arrayField)) {
            throw new ExecuteNodeException("convertToObjectArray arrayField field is required!");
        }

        if (StrUtil.isBlank(keyField)) {
            keyField = "key";
        }

        JSONObject outputJson = input.getData().clone();
        Object list = outputJson.getByPath(arrayField);
        if (list instanceof JSONArray) {
            JSONArray array = (JSONArray) list;
            JSONArray newArray = new JSONArray();
            for (Object item : array) {
                JSONObject jsonObject = new JSONObject(1);
                jsonObject.put(keyField, item);
                newArray.add(jsonObject);
            }
            JSONPath.set(outputJson, arrayField, newArray);
        }
        output.setData(outputJson);
    }
}
