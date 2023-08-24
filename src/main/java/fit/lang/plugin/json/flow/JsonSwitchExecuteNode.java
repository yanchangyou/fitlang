package fit.lang.plugin.json.flow;

import com.alibaba.fastjson.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.ExecuteNodeEngineConst;
import fit.lang.ExecuteNodeException;
import fit.lang.common.flow.SwitchExecuteNode;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.define.base.ExecuteNodeInput;

import static fit.lang.ExecuteNodeEngineConst.*;

/**
 * 执行节点
 */
public class JsonSwitchExecuteNode extends SwitchExecuteNode implements ExecuteNodeBuildable {

    String switchField;

    public String getSwitchField() {
        return switchField;
    }

    public void setSwitchField(String switchField) {
        this.switchField = switchField;
    }

    @Override
    public String getCaseValue(ExecuteNodeInput input) {
        return ((JsonExecuteNodeInput) input).getString(switchField);
    }

    @Override
    public void build(ExecuteNodeData nodeDefineData) {

        JSONObject nodeDefine = ((JsonExecuteNodeData) nodeDefineData).getData();
        ExecuteNodeUtil.buildChildNode(this, nodeDefine);

        switchField = nodeDefine.getString(DEFINE_KEYWORDS_OF_SWITCH_FIELD_NAME);
        if (switchField == null) {
            throw new ExecuteNodeException("switch node switchField is empty!");
        }

        if (childNodes.isEmpty()) {

            JSONObject caseObject = nodeDefine.getJSONObject("case");
            if (caseObject == null) {
                throw new ExecuteNodeException("switch node case field is empty and child is empty!");
            }

            for (String caseValue : caseObject.keySet()) {
                JSONObject caseNodeDefine = caseObject.getJSONObject(caseValue);
                ExecuteNode caseNode = JsonDynamicFlowExecuteEngine.createExecuteNode(caseNodeDefine);
                addCaseNode(caseValue, caseNode);
                addChildNode(caseNode);
            }

        } else {
            for (ExecuteNode child : childNodes) {
                JsonExecuteNodeData childDefine = (JsonExecuteNodeData) child.getNodeDefine();
                String caseValue = childDefine.getString(ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_SWITCH_CASE_VALUE);
                if (caseValue == null) {
                    throw new ExecuteNodeException("switch node case value is required, node : " + childDefine.getString(DEFINE_KEYWORDS_OF_UNI) + "." + childDefine.getString(DEFINE_KEYWORDS_OF_ID));
                }
                addCaseNode(caseValue, child);
            }
        }
    }
}
