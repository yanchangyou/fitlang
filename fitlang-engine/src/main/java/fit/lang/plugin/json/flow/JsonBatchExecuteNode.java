package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.define.ExecuteNodeData;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public class JsonBatchExecuteNode extends JsonSequenceExecuteNode {

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        JSONObject nodeDefineJson = getJsonData(executeNodeData);

        setBagsMode(true);
        setBagsName("list");

        ExecuteNodeUtil.buildChildNode(this, nodeDefineJson);
    }

}
