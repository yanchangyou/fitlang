package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.base.*;
import fit.lang.plugin.json.define.JsonExecuteContext;

/**
 * 执行节点
 */
public class JsonNodeExecuteNode extends AbstractExecuteNode implements ExecuteNodeBuildable {

    public JsonNodeExecuteNode() {

    }

    public JsonNodeExecuteNode(JsonExecuteContext nodeContext) {
        setNodeContext(nodeContext);
    }

    String nodePath;

    public String getNodePath() {
        return nodePath;
    }

    public void setNodePath(String nodePath) {
        this.nodePath = nodePath;
    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        nodePath = uni.substring(5);
    }

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        JSONObject nodeDefine = (JSONObject) this.getNodeDefine().getData();

        //涉及uni变化逻辑处理
        String oldUni = nodeDefine.getString("uni");
        nodeDefine.put("uni", nodePath);
        ExecuteNode executeNode = JsonDynamicFlowExecuteEngine.createExecuteNode(nodePath, (JSONObject) this.getNodeDefine().getData(), nodeContext);
        nodeDefine.put("uni", oldUni);

        executeNode.execute(input, output);
    }
}
