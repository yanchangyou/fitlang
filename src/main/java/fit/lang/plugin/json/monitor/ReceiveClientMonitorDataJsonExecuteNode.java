package fit.lang.plugin.json.monitor;

import cn.hutool.core.util.StrUtil;
import cn.hutool.crypto.SecureUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static fit.lang.ExecuteNodeUtil.getNow;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.buildInnerClientId;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.buildInnerClientIp;

/**
 * 接收监控数据
 */
public class ReceiveClientMonitorDataJsonExecuteNode extends JsonExecuteNode {

    static Map<String, JSONObject> clientInfoMap = new HashMap<>();
    static List<JSONObject> clients = new ArrayList<>();

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject result = new JSONObject();

        String clientId = parseStringField("clientId", input);

        if (StrUtil.isBlank(clientId)) {
            result.put("message", "clientId is empty!");
            output.setData(result);
            return;
        }

        String clientIp = parseStringField("clientIp", input);

        //clientIp 敏感信息md5加密
        clientIp = buildInnerClientIp(clientIp);

        String innerClientId = buildInnerClientId(clientId, clientIp);

        String cpuTotal = input.getString("cpuTotal");
        JSONObject clientInfo = input.getJsonObject("clientInfo");
        JSONObject client = clientInfoMap.get(innerClientId);
        if (client == null) {
            client = new JSONObject();
            clientInfoMap.put(innerClientId, client);
            client.put("cpuPoints", new JSONArray());
            client.put("memoryPoints", new JSONArray());
            client.put("cpuTotal", cpuTotal);
            client.put("clientInfo", clientInfo);

            JSONObject onlyClientInfo = new JSONObject();
            onlyClientInfo.put("clientId", clientId);
            onlyClientInfo.put("innerClientId", innerClientId);
            onlyClientInfo.put("clientIp", clientIp);
            onlyClientInfo.put("startTime", getNow());
            onlyClientInfo.put("clientInfo", clientInfo);

            clients.add(onlyClientInfo);
        }

        JSONObject cpuPoint = input.getJsonObject("cpuPoint");
        JSONObject memoryPoint = input.getJsonObject("memoryPoint");

        if (cpuPoint != null && !cpuPoint.isEmpty() && memoryPoint != null && !memoryPoint.isEmpty()) {
            //补充缺失的数据
            if (!cpuPoint.containsKey("timestamp")) {
                cpuPoint.put("timestamp", System.currentTimeMillis());
                cpuPoint.put("timestampShow", getNow());
            }
            client.getJSONArray("cpuPoints").add(cpuPoint);

            //补充缺失的数据
            if (!memoryPoint.containsKey("timestamp")) {
                memoryPoint.put("timestamp", System.currentTimeMillis());
                memoryPoint.put("timestampShow", getNow());
            }
            client.getJSONArray("memoryPoints").add(memoryPoint);

            client.put("cpuTotal", cpuTotal);

        } else {
            client.put("message", "cpuPoint or memoryPoint data is empty!");
        }

        client.put("clientId", clientId);
        client.put("clientIp", clientIp);
        client.put("innerClientId", innerClientId);

        output.setData(client);
    }

    public static List<JSONObject> getClients() {
        return clients;
    }

    public static JSONObject getClient(String innerClientId) {
        return clientInfoMap.get(innerClientId);
    }

}
