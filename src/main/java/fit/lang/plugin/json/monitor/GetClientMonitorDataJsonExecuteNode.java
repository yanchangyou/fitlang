package fit.lang.plugin.json.monitor;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.monitor.StartMonitorJsonExecuteNode.getGatherList;

/**
 * 获取cloud场景下，获取client推送的监控信息
 */
public class GetClientMonitorDataJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        int second = parseIntField("second", input, 500);

        JSONObject result = new JSONObject();

        String clientId = parseStringField("clientId", input);

        result.put("clientId", clientId);
        result.put("second", second);

        if (StrUtil.isNotBlank(clientId)) {
            JSONObject client = ReceiveClientMonitorDataJsonExecuteNode.getClient(clientId);
            if (client == null) {
                result.put("message", "clientId not existed!");
            } else {
                result.put("cpuPoints", getGatherList(client.getJSONArray("cpuPoints"), second));
                result.put("memoryPoints", getGatherList(client.getJSONArray("memoryPoints"), second));
                result.put("clientInfo", client.get("clientInfo"));
                result.put("cpuTotal", client.get("cpuTotal"));
            }
        } else {
            result.put("message", "clientId is required!");
        }

        output.setData(result);
    }
}
