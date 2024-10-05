package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.define.ExecuteNodeBuildable;
import fit.lang.define.ExecuteNodeData;
import fit.lang.plugin.json.JsonConvertUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public class ConvertJsonExecuteNode extends JsonExecuteNode implements ExecuteNodeBuildable {


    /**
     * 表达式
     */
    JSONObject express = new JSONObject();

    /**
     * 值映射
     */
    JSONObject valueMapping = new JSONObject();

    public JSONObject getExpress() {
        return express;
    }

    public void setExpress(JSONObject express) {
        this.express = express;
    }

    public JSONObject getValueMapping() {
        return valueMapping;
    }

    public void setValueMapping(JSONObject valueMapping) {
        this.valueMapping = valueMapping;
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        if (nodeJsonDefine != null) {
            Boolean isMixMode = nodeJsonDefine.getBoolean("isMixMode");
            if (Boolean.TRUE.equals(isMixMode)) {
                output.setData(input.getData().clone());
            }
        }

        JSONObject convertConfig = new JSONObject();
        convertConfig.put("valueMapping", valueMapping);

        JsonConvertUtil.convertJson(input.getData(), express, output.getData(), convertConfig);
    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {

        setExpress(getJsonData(executeNodeData).getJSONObject("express"));

        if (express == null) {
            throw new ExecuteNodeException("convert express field is required!");
        }
        setValueMapping(getJsonData(executeNodeData).getJSONObject("valueMapping"));
    }
}