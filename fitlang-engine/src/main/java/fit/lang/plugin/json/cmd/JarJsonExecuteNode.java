package fit.lang.plugin.json.cmd;

import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class JarJsonExecuteNode extends CmdJsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        nodeJsonDefine.put("cmd", "jar");
        super.execute(input, output);
    }

}
