package fit.lang.plugin.json.monitor;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.List;

import static fit.lang.plugin.json.monitor.ReceiveClientMonitorDataJsonExecuteNode.getClients;

/**
 * 执行节点
 */
public class GetMonitorClientJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject result = new JSONObject();

        List clients = getClients();
        result.put("clients", clients);

        output.setData(result);
    }
}
