package fit.lang.plugin.json.flow;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class CallJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String nodeId = nodeJsonDefine.getString("nodeId");
        if (StrUtil.isBlank(nodeId)) {
            throw new ExecuteNodeException("call node id is required!");
        }
        Object nodeDefine = input.getNodeContext().getNode(nodeId);
        if (!(nodeDefine instanceof JSONObject)) {
            throw new ExecuteNodeException("node is not existed by node id: ".concat(nodeId));
        }

        String result = ExecuteJsonNodeUtil.executeCode(input.getData(), (JSONObject) nodeDefine, new JSONObject(), input.getNodeContext());

        output.setData(JSONObject.parseObject(result));

    }
}
