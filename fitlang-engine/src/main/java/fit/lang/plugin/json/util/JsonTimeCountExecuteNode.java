package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.common.flow.WrapExecuteNode;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public class JsonTimeCountExecuteNode extends WrapExecuteNode implements ExecuteNodeBuildable {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {
        super.execute(input, output);
        Object result = output.getNodeData().getData();
        JSONObject realResult = new JSONObject();
        realResult.put("output", result);
        realResult.put("executeInfo", JSONObject.from(getNodeExecuteInfo()));
        output.getNodeData().setData(realResult);
    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        JSONObject nodeDefine = getJsonData(executeNodeData);
        ExecuteNodeUtil.buildChildNode(this, nodeDefine);
    }
}
