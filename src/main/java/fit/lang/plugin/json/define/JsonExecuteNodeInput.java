package fit.lang.plugin.json.define;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.base.ExecuteNodeInput;

import java.util.Map;

/**
 * 执行节点入参
 */
public class JsonExecuteNodeInput extends JsonExecuteNodePut implements ExecuteNodeInput {

    public JsonExecuteNodeInput(JsonExecuteContext nodeContext) {
        this(new JsonExecuteNodeData(), nodeContext);
    }

    public JsonExecuteNodeInput(JsonExecuteNodeData data, JsonExecuteContext nodeContext) {
        super(data, nodeContext);
    }

    /**
     * 整合入参和上下文参数
     *
     * @return
     */
    public JSONObject getInputParamAndContextParam() {
        JSONObject param = getData().clone();
        param.put("context", getNodeContext().getAllAttribute());
        Map<String, Object> attribute = getNodeContext().getAllAttribute();
        for (Map.Entry<String, Object> item : attribute.entrySet()) {
            param.putIfAbsent(item.getKey(), item.getValue());
        }
        return param;
    }

    @Override
    public ExecuteNodeInput createInput() {
        return new JsonExecuteNodeInput(this.nodeContext);
    }
}