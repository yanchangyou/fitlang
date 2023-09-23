package fit.lang.plugin.json.monitor;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.covertToG;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.covertToLong;
import static fit.lang.plugin.json.monitor.JsonExecuteNodeMonitorUtil.*;

/**
 * 获取cloud场景下，获取client推送的监控信息
 */
public class GetClientMonitorDataJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject result = new JSONObject();

        String innerClientId = parseStringField("innerClientId", input);

        if (StrUtil.isBlank(innerClientId)) {
            result.put("message", "innerClientId is required!");
            output.setData(result);
            return;
        }

        int second = parseIntField("second", input, 500);

        result.put("clientId", innerClientId);
        result.put("second", second);

        JSONObject client = ReceiveClientMonitorDataJsonExecuteNode.getClient(innerClientId);
        if (client == null) {
            result.put("message", "client not existed!");
        } else {
            result.put("cpuPoints", fetchMonitorDataInLastSecond(client.getJSONArray("cpuPoints"), second));
            result.put("memoryPoints", fetchMonitorDataInLastSecond(client.getJSONArray("memoryPoints"), second));
            result.put("clientInfo", client.get("clientInfo"));
            result.put("cpuTotal", client.get("cpuTotal"));
            result.put("sumCpuPoints", covertToLong(covertToG(sumCpuDataInLastSecond(client.getJSONArray("cpuPoints"), second, JsonExecuteNodeMonitorUtil.getCpuTotal()))));
            result.put("sumMemoryPoints", covertToLong(sumMemoryDataInLastSecond(client.getJSONArray("memoryPoints"), second)));
        }

        output.setData(result);
    }
}
