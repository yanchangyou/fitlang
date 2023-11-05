package fit.lang.plugin.json.flow;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.function.JsonPackageExecuteNode;

/**
 * 执行节点
 */
public class CallJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String nodeId = nodeJsonDefine.getString("nodeId");
        String functionId = nodeJsonDefine.getString("function");
        if (StrUtil.isBlank(nodeId) && StrUtil.isBlank(functionId)) {
            throw new ExecuteNodeException("call nodeId or function field is required!");
        }
        if (StrUtil.isBlank(functionId)) {
            Object nodeDefine = input.getNodeContext().getNode(nodeId);
            if (!(nodeDefine instanceof JSONObject)) {
                throw new ExecuteNodeException("node is not existed by node id: ".concat(nodeId));
            }
            String result = ExecuteJsonNodeUtil.executeCode(input.getData(), (JSONObject) nodeDefine, new JSONObject(), input.getNodeContext());
            output.setData(JSONObject.parseObject(result));
        } else {
            JsonExecuteNode functionNode = JsonPackageExecuteNode.getFunction(functionId);
            if(functionNode == null) {
                throw new ExecuteNodeException("function is not existed: ".concat(functionId));
            }
            functionNode.execute(input, output);
        }
    }
}
