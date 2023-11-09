package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.common.flow.WrapExecuteNode;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.info.NodeExecuteInfo;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public class PerformanceJsonExecuteNode extends WrapExecuteNode implements ExecuteNodeBuildable {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {
        super.execute(input, output);
        Object result = output.getNodeData().getData();
        JSONObject realResult = new JSONObject();
        realResult.put("output", result);
        getNodeExecuteInfo().evalTps(NodeExecuteInfo.globalNodeExecuteInfo.getTotal());
        NodeExecuteInfo.globalNodeExecuteInfo.setTotal(0);

        realResult.put("executeInfo", JSONObject.from(getNodeExecuteInfo()));
        output.getNodeData().setData(realResult);
    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        JSONObject nodeDefine = getJsonData(executeNodeData);
        ExecuteNodeUtil.buildChildNode(this, nodeDefine);
    }
}
