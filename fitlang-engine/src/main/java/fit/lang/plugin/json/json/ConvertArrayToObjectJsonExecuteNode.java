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
public class ConvertArrayToObjectJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String arrayField = nodeJsonDefine.getString("arrayField");
        String objectField = nodeJsonDefine.getString("objectField");
        String keyField = nodeJsonDefine.getString("keyField");
        String valueField = nodeJsonDefine.getString("valueField");

        if (StrUtil.isBlank(objectField)) {
            objectField = arrayField;
        }

        if (StrUtil.isBlank(arrayField)) {
            arrayField = "array";
        }

        if (StrUtil.isBlank(keyField)) {
            keyField = "key";
        }

        JSONObject outputJson = input.getData().clone();
        Object list = outputJson.getByPath(arrayField);
        if (list instanceof JSONArray) {
            JSONArray array = (JSONArray) list;
            JSONObject jsonObject = new JSONObject();
            for (Object item : array) {
                if (!(item instanceof JSONObject)) {
                    throw new ExecuteNodeException("convertArrayToObject item must be object type!");
                }
                JSONObject itemObject = (JSONObject) item;
                Object value = itemObject;
                if (StrUtil.isNotBlank(valueField)) {
                    value = itemObject.get(valueField);
                }
                jsonObject.put(itemObject.getString(keyField), value);
            }
            JSONPath.set(outputJson, objectField, jsonObject);
        }
        output.setData(outputJson);
    }
}
