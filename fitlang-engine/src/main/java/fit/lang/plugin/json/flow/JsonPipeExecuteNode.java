package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.common.flow.PipeExecuteNode;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeBuildable;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public class JsonPipeExecuteNode extends PipeExecuteNode implements ExecuteNodeBuildable {

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        JSONObject nodeDefine = getJsonData(executeNodeData);
        ExecuteNodeUtil.buildChildNode(this, nodeDefine);
    }
}
