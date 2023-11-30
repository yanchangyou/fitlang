package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.common.flow.WrapExecuteNode;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;

import static fit.lang.ExecuteNodeUtil.getAllException;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public class CatchJsonExecuteNode extends WrapExecuteNode implements ExecuteNodeBuildable {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {
        try {
            super.execute(input, output);
        } catch (Throwable e) {
            JSONObject outData = new JSONObject();
            outData.put("exception", getAllException(e));
            output.getNodeData().setData(outData);
        }
    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        JSONObject nodeDefine = getJsonData(executeNodeData);
        ExecuteNodeUtil.buildChildNode(this, nodeDefine);
    }
}
