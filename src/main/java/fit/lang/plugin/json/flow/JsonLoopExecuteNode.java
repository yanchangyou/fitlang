package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.common.flow.LoopExecuteNode;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeBuildable;

/**
 * 执行节点
 */
public class JsonLoopExecuteNode extends LoopExecuteNode implements ExecuteNodeBuildable {

    @Override
    public void build(ExecuteNodeData executeNodeData) {

        JSONObject nodeDefine = ((JsonExecuteNodeData) executeNodeData).getData();

        setLoopTimes(nodeDefine.getInteger("loopTimes"));

        ExecuteNodeUtil.buildChildNode(this, nodeDefine);
    }

}
