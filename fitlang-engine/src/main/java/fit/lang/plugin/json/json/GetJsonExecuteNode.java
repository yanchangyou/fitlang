package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class GetJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String path = nodeJsonDefine.getString("path");
        Object object = input.getInputParamAndContextParam().getByPath(path);
        JSONObject data;

        if (object instanceof JSONObject) {
            data = (JSONObject) object;
        } else {
            data = new JSONObject(1);
            data.put("_raw", object);
        }

        output.setData(data);
    }
}
