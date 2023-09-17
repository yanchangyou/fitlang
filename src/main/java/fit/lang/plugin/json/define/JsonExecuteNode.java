package fit.lang.plugin.json.define;

import cn.hutool.core.util.NumberUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;

/**
 * 执行节点
 */
public abstract class JsonExecuteNode extends AbstractExecuteNode {

    protected JSONObject nodeJsonDefine;

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

    protected int parseIntField(String fieldName, JsonExecuteNodeInput input, int defaultValue) {
        String resultText = parseStringField("fieldName", input);

        int result = defaultValue;
        if (resultText != null && NumberUtil.isInteger(resultText)) {
            result = Integer.parseInt(resultText);
        }
        return result;
    }

}
