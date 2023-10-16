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
public class ConvertKeyValueListJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String listField = nodeJsonDefine.getString("listField");
        String keyField = nodeJsonDefine.getString("keyField");
        String valueField = nodeJsonDefine.getString("valueField");

        if (StrUtil.isBlank(listField)) {
            throw new ExecuteNodeException(" convertKeyValueList listField field is required!");
        }

        if (StrUtil.isBlank(keyField)) {
            throw new ExecuteNodeException(" convertKeyValueList keyField field is required!");
        }

        if (StrUtil.isBlank(valueField)) {
            throw new ExecuteNodeException(" convertKeyValueList valueField field is required!");
        }

        JSONObject outputJson = input.getData().clone();
        Object list = outputJson.getByPath(listField);
        if (list instanceof JSONArray) {
            JSONArray array = (JSONArray) list;
            JSONObject jsonObject = new JSONObject();
            for (Object item : array) {
                if (!(item instanceof JSONObject)) {
                    throw new ExecuteNodeException(" convertKeyValueList item must be object type!");
                }
                JSONObject itemObject = (JSONObject) item;
                jsonObject.put(itemObject.getString(keyField), itemObject.get(valueField));
            }
            JSONPath.set(outputJson, listField, jsonObject);

        }
        output.setData(outputJson);
    }
}
