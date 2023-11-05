package fit.lang.plugin.json.function;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.ExecuteNodeUtil;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.define.base.*;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;


/**
 * 执行节点
 */
public class JsonFunctionExecuteNode extends JsonExecuteNode implements ExecuteNodeBuildable {

    String functionName;

    /**
     * 静态解析
     *
     * @param executeNodeData
     */
    @Override
    public void build(ExecuteNodeData executeNodeData) {

        JSONObject nodeDefineJson = (JSONObject) nodeDefine.getData();
        functionName = nodeDefineJson.getString("name");
        if (StrUtil.isBlank(functionName)) {
            throw new ExecuteNodeException("function name field is required!");
        }

        ExecuteNodeUtil.buildChildNode(this, nodeDefineJson);
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        for (ExecuteNode childNode : childNodes) {
            if (this.isNeedCloneInputData()) {
                input.getNodeData().setData(input.getNodeData().cloneData());
            }
            childNode.executeAndNext(input, output);
        }

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }

}
