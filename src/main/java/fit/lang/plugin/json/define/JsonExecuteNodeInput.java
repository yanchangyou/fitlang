package fit.lang.plugin.json.define;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.base.ExecuteNodeInput;

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
        return param;
    }
}