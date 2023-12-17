package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.ExecuteNodeUtil;
import fit.lang.common.flow.WrapExecuteNode;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;

import static fit.lang.ExecuteNodeUtil.getRootException;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public class CatchJsonExecuteNode extends WrapExecuteNode implements ExecuteNodeBuildable {

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {
        try {
            getChildNodes().get(0).execute(input, output);
        } catch (Throwable e) {
            JSONObject exceptionData = new JSONObject();
            exceptionData.put("exception", getRootException(e));
            exceptionData.put("input", input.getNodeData().getData());
            input.getNodeData().setData(exceptionData);
            getChildNodes().get(1).execute(input, output);
        }
    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        JSONObject nodeDefine = getJsonData(executeNodeData);
        ExecuteNodeUtil.buildChildNode(this, nodeDefine);
        if (getChildNodes().size() != 2) {
            throw new ExecuteNodeException("catch node the children node must be only 2 child; The first is try node, the second is catch node!");
        }
    }
}
