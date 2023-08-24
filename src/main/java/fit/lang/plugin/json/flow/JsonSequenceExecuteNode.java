package fit.lang.plugin.json.flow;

import com.alibaba.fastjson.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.common.flow.SequenceExecuteNode;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.define.base.ExecuteNodeData;

/**
 * 执行节点
 */
public class JsonSequenceExecuteNode extends SequenceExecuteNode implements ExecuteNodeBuildable {

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        JSONObject nodeDefine = ((JsonExecuteNodeData) executeNodeData).getData();
        ExecuteNodeUtil.buildChildNode(this, nodeDefine);
    }
}
