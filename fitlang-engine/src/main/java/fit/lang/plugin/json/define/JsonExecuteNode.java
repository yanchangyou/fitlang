package fit.lang.plugin.json.define;

import cn.hutool.core.util.NumberUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.ExecuteNodeData;
import fit.lang.define.ExecuteNodeInput;
import fit.lang.define.ExecuteNodeOutput;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;

import java.util.List;

/**
 * 执行节点
 */
public abstract class JsonExecuteNode extends AbstractExecuteNode {

    protected JSONObject nodeJsonDefine;

    public JsonExecuteContext getNodeContext() {
        return (JsonExecuteContext) nodeContext;
    }

    public void setNodeContext(JsonExecuteContext nodeContext) {
        super.setNodeContext(nodeContext);
    }

    public void setNodeDefine(JSONObject nodeJsonDefine) {
        super.setNodeDefine(new JsonExecuteNodeData(nodeJsonDefine));
        this.nodeJsonDefine = nodeJsonDefine;
    }

    public void setNodeDefine(ExecuteNodeData nodeDefine) {
        super.setNodeDefine(nodeDefine);
        nodeJsonDefine = (JSONObject) (nodeDefine.getData());
    }

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

//        this.setNodeContext(input.getNodeContext());

        execute((JsonExecuteNodeInput) input, (JsonExecuteNodeOutput) output);

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }

    public abstract void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output);

    /**
     * 解析字段值: 先从入参获取，然后从配置中获取
     *
     * @param fieldName
     * @param input
     * @return
     */
    protected String parseStringField(String fieldName, JsonExecuteNodeInput input) {
        return ExecuteJsonNodeUtil.parseStringField(fieldName, input, nodeJsonDefine);
    }

    /**
     * 判断是否数组字段
     *
     * @param fieldName
     * @param input
     * @return
     */
    protected boolean isArrayField(String fieldName, JsonExecuteNodeInput input) {
        return ExecuteJsonNodeUtil.isArrayField(fieldName, input, nodeJsonDefine);
    }

    /**
     * 解析字段值: 先从入参获取，然后从配置中获取
     *
     * @param fieldName
     * @param input
     * @return
     */
    protected List<String> parseStringArray(String fieldName, JsonExecuteNodeInput input) {
        return ExecuteJsonNodeUtil.parseStringArray(fieldName, input, nodeJsonDefine);
    }

    protected int parseIntField(String fieldName, JsonExecuteNodeInput input, int defaultValue) {
        String resultText = parseStringField(fieldName, input);

        int result = defaultValue;
        if (NumberUtil.isInteger(resultText)) {
            result = Integer.parseInt(resultText);
        }
        return result;
    }

    protected Object parseField(String fieldName, JsonExecuteNodeInput input) {
        return ExecuteJsonNodeUtil.parseField(fieldName, input, nodeJsonDefine);
    }
}
