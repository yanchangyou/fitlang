package fit.lang.plugin.json.util;

import cn.hutool.core.lang.Dict;
import cn.hutool.extra.expression.engine.aviator.AviatorEngine;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.define.JsonExecuteNode;

/**
 * 执行节点
 */
public class HelloJsonExecuteNode extends JsonExecuteNode {

    AviatorEngine aviatorEngine = new AviatorEngine();

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        String message = nodeJsonDefine.getString("message");
        String who = input.getString("who");
        if (who == null) {
            who = nodeJsonDefine.getString("who");
        }
        if (who == null) {
            who = "world";
        }
        if (message == null) {
            message = "hello, " + who + "!";
        } else if (message.startsWith("${") && message.endsWith("}")) {
            Dict dict = Dict.create().set("who", who);
            message = aviatorEngine.eval(message.substring(2, message.length() - 1), dict, null).toString();
        }

        output.set("message", message);

    }
}
