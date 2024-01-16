package fit.lang.plugin.json.json;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONPath;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.Map;

/**
 * 执行节点
 */
public class ConvertObjectToArrayJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String arrayField = nodeJsonDefine.getString("arrayField");
        String objectField = nodeJsonDefine.getString("objectField");
        String keyField = nodeJsonDefine.getString("keyField");
        String valueField = nodeJsonDefine.getString("valueField");

        if (StrUtil.isBlank(objectField)) {
            throw new ExecuteNodeException(" convertObjectToArray objectField field is required!");
        }

        if (StrUtil.isBlank(arrayField)) {
            arrayField = "array";
        }

        if (StrUtil.isBlank(keyField)) {
            keyField = "key";
        }

        if (StrUtil.isBlank(valueField)) {
            valueField = "value";
        }

        JSONObject outputJson = input.getData().clone();
        Object object = outputJson.getByPath(objectField);
        JSONArray array = new JSONArray();
        if (object instanceof JSONObject) {
            JSONObject jsonObject = (JSONObject) object;
            for (Map.Entry<String, Object> item : jsonObject.entrySet()) {
                JSONObject itemObject = new JSONObject(2);
                itemObject.put(keyField, item.getKey());
                itemObject.put(valueField, item.getValue());
                array.add(itemObject);
            }
            JSONPath.set(outputJson, arrayField, array);
        }
        output.setData(outputJson);
    }
}
