package fit.lang.plugin.json.json;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getWildlyField;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.parseJsonSchema;
import static fit.lang.plugin.json.json.SetJsonExecuteNode.WILDLY_FIELDS;

/**
 * 执行节点
 */
public class GetSchemaJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String path = getWildlyField(nodeJsonDefine, WILDLY_FIELDS);

        JSONObject jsonObject;

        if (StrUtil.isBlank(path)) {
            jsonObject = input.getData();
        } else {
            path = (String) ExpressUtil.eval(path, input.getInputParamAndContextParam());

            Object object = input.getInputParamAndContextParam().getByPath(path);
            if (object instanceof JSONObject) {
                jsonObject = (JSONObject) object;
            } else {
                jsonObject = new JSONObject(1);
                jsonObject.put("_raw", object);
            }
        }

        JSONObject schema = new JSONObject();
        schema.put("schema", parseJsonSchema(jsonObject));

        output.setData(schema);
    }
}
