package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.define.base.*;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.ExecuteNodeEngineConst;
import fit.lang.ExecuteNodeException;
import fit.lang.common.flow.SwitchExecuteNode;

import static fit.lang.ExecuteNodeEngineConst.*;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.parseStringExcludeContext;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.parseStringField;
import static fit.lang.plugin.json.ExpressUtil.eval;

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
        String caseValue;
        //动态解析
        if (switchField.startsWith("${")) {
            caseValue = parseStringExcludeContext(switchField, (JsonExecuteNodeInput) input);
        } else {
            caseValue = parseStringField(switchField, (JsonExecuteNodeInput) input, (JSONObject) nodeDefine.getData());
        }
        if (caseValue == null) {
            return null;
        }
        caseValue = eval(caseValue, ((JsonExecuteNodeInput) input).getInputParamAndContextParam()).toString();
        return caseValue;
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

    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        super.execute(input, output);
    }
}
