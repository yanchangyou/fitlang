package fit.lang.plugin.json.cloud;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.List;

/**
 * 执行节点
 */
public class CloudGetClientJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject result = new JSONObject();

        String clientId = parseStringField("clientId", input);

        List clients = CloudServerJsonExecuteNode.getSessions();
        result.put("clients", clients);

        JSONObject client = CloudServerJsonExecuteNode.getSession(clientId);
        result.put("clientId", clientId);
        result.put("client", client);

        output.setData(result);
    }
}
