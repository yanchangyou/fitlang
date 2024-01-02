package fit.lang.plugin.json.util;

import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class SetGlobalJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String key = nodeJsonDefine.getString("key");
        Object value = nodeJsonDefine.get("value");

        Object newValue = ExpressUtil.eval(value, input.getInputParamAndContextParam());
        input.getNodeContext().setAttribute(key, newValue);

        output.setData(input.getData().clone());

    }
}
