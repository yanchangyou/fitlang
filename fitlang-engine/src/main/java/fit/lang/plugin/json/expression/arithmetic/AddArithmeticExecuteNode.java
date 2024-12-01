package fit.lang.plugin.json.expression.arithmetic;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.expression.ExpressExecuteNode;

/**
 * 执行节点
 */
public class AddArithmeticExecuteNode extends ExpressExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String who = parseStringField("who", input);

        if (who == null) {
            who = "world";
        }

        String message = parseStringField("message", input);

        if (message == null) {
            message = "hello, " + who + "!";
        }
        output.set("message", message);

    }

    @Override
    public JSONObject eval(JSONObject express) {

        return null;
    }
}
