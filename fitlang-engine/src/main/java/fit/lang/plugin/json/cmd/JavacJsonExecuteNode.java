package fit.lang.plugin.json.cmd;

import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class JavacJsonExecuteNode extends CmdJsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        nodeJsonDefine.put("cmd", "javac");
        super.execute(input, output);
    }

}
