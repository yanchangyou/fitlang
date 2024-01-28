package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.common.flow.SequenceExecuteNode;
import fit.lang.define.ExecuteNodeBuildable;
import fit.lang.define.ExecuteNodeData;
import fit.lang.define.ExecuteNodeOutput;
import fit.lang.plugin.json.define.JsonExecuteNodeData;

import java.util.List;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public class JsonSequenceExecuteNode extends SequenceExecuteNode implements ExecuteNodeBuildable {

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        JSONObject nodeDefineJson = getJsonData(executeNodeData);

        setBagsMode(Boolean.TRUE.equals(nodeDefineJson.getBoolean("isBagsMode")));
        setBagsName(nodeDefineJson.getString("bagsName"));

        ExecuteNodeUtil.buildChildNode(this, nodeDefineJson);
    }

    @Override
    public void setBags(String bagsFieldName, List list, ExecuteNodeOutput output) {
        JsonExecuteNodeData jsonExecuteNodeData = (JsonExecuteNodeData) output.getNodeData();
        jsonExecuteNodeData.getData().clear();
        jsonExecuteNodeData.getData().put(bagsFieldName, list);
    }
}
