package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.common.flow.LoopExecuteNode;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeBuildable;

import java.util.List;

/**
 * 执行节点
 */
public class JsonLoopExecuteNode extends LoopExecuteNode implements ExecuteNodeBuildable {

    @Override
    public void build(ExecuteNodeData executeNodeData) {

        JSONObject nodeDefine = ((JsonExecuteNodeData) executeNodeData).getData();

        setLoopTimes(nodeDefine.getInteger("loopTimes"));
        setPipe(Boolean.TRUE.equals(nodeDefine.getBoolean("isPipe")));
        setBagsMode(Boolean.TRUE.equals(nodeDefine.getBoolean("isBagsMode")));
        setBagsName(nodeDefine.getString("bagsName"));

        ExecuteNodeUtil.buildChildNode(this, nodeDefine);
    }

    @Override
    public List getBags(int size) {
        return new JSONArray(size);
    }

    @Override
    public void setBags(String bagsFieldName, List list, ExecuteNodeOutput output) {
        JSONObject data = new JSONObject();
        data.put(bagsFieldName, list);
        ((JsonExecuteNodeData) output.getNodeData()).setData(data);
    }
}
