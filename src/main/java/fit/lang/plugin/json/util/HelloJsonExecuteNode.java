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
        String message = null;
        if (nodeJsonDefine != null && nodeJsonDefine.get("message") != null) {
            message = nodeJsonDefine.getString("message");
        }
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
            JSONObject param = new JSONObject();
            param.put("who", who);
            message = ExpressUtil.eval(message, param).toString();
        }

        output.set("message", message);

    }
}
