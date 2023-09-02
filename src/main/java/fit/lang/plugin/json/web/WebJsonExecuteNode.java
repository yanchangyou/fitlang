package fit.lang.plugin.json.web;

import fit.lang.ExecuteNodeUtil;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class WebJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        ExecuteNodeUtil.buildChildNode(this, nodeJsonDefine);

        if (childNodes != null && !childNodes.isEmpty()) {
            childNodes.get(0).executeAndNext(input, output);
        }

        ExecuteNodeSimpleAop.afterExecute(input, this, output);
    }
}
