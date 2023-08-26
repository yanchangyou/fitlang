package fit.lang.plugin.json.util;

import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.define.JsonExecuteNode;

/**
 * 执行节点
 */
public class HelloJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        String who = input.getString("who");
        if (who == null) {
            who = nodeJsonDefine.getString("who");
        }
        if (who == null) {
            who = "world";
        }
        String message = "hello, " + who + "!";

        output.set("message", message);

    }
}
