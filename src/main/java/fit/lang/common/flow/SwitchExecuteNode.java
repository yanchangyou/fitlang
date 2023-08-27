package fit.lang.common.flow;

import fit.lang.ExecuteNodeUtil;
import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.ExecuteNodeException;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.common.AbstractExecuteNode;

import java.util.HashMap;
import java.util.Map;

/**
 * 执行节点
 * 两种定义格式
 * 1，简洁格式：case : {'caseValue1':{'uni':'xxx'},'caseValue2':{'uni':'yyy'}} (不建议使用)
 * 2，统一格式：switchField:'fieldName', child: [{'case':'caseValue1','uni':'xxx'},{'case':'caseValue2','uni':'yyy'}]
 */
public abstract class SwitchExecuteNode extends AbstractExecuteNode {

    Map<String, ExecuteNode> caseNodeMap = new HashMap<>();

    public void addCaseNode(String caseValue, ExecuteNode node) {
        caseNodeMap.put(caseValue, node);
    }

    public ExecuteNode getCaseNode(String caseValue) {
        return caseNodeMap.get(caseValue);
    }

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {
        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        String caseValue = getCaseValue(input);

        if (caseValue == null) {
            caseValue = "default";
        }

        ExecuteNode caseNode = getCaseNode(caseValue);

        if (caseNode == null) {
            throw new ExecuteNodeException("switch case node is empty: " + ExecuteNodeUtil.getExecuteNodeBasicInfo(this));
        }
        caseNode.executeAndNext(input, output);

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }

    public abstract String getCaseValue(ExecuteNodeInput input);

}
