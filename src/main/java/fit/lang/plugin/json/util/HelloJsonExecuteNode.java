package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.define.JsonExecuteNode;

/**
 * 执行节点
 */
public class HelloJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        String who = parseStringField("who", input);
        if (who == null) {
            who = "world";
        }
        input.set("who", who);
        String message = parseStringField("message", input);

        if (message == null) {
            message = "hello, " + who + "!";
        }
        output.set("message", message);

    }
}
