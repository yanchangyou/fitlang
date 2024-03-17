package fit.lang.plugin.json.ide.jcef;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class JcefOpenJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        String url = ExecuteJsonNodeUtil.parseStringField("url", input, nodeJsonDefine);
        FitJcefManager.open(url);
        output.set("url", url);
    }
}
