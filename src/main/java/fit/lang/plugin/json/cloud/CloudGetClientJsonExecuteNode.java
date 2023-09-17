package fit.lang.plugin.json.cloud;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class CloudGetClientJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject result = new JSONObject();

        String clientId = parseStringField("clientId", input);

        result.put("clientId", clientId);
        JSONObject client = CloudServerJsonExecuteNode.getSession(clientId);
        if (client == null) {
            result.put("message", "client not existed!");
        } else {
            result.put("client", client);
        }
        output.setData(result);
    }
}
