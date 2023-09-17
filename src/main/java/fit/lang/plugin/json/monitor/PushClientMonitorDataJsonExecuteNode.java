package fit.lang.plugin.json.monitor;

import cn.hutool.core.util.StrUtil;
import cn.hutool.http.HttpUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.monitor.StartMonitorJsonExecuteNode.*;

/**
 * 推送监控数据
 */
public class PushClientMonitorDataJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        String clientId = parseStringField("clientId", input);
        JSONObject pushData = pushMonitorData(nodeJsonDefine.get("pushUrl"), clientId, input.getData());
        pushData.put("pushUrl", nodeJsonDefine.get("pushUrl"));

        output.setData(pushData);
    }

    public static JSONObject pushMonitorData(Object pushUrl, String clientId, JSONObject pushData) {
        JSONObject pushResult;
        if (pushUrl instanceof JSONArray) {
            JSONArray pushUrls = (JSONArray) pushUrl;
            pushResult = pushMonitorData(pushUrls.toArray(new String[0]), clientId, pushData);
        } else if (pushUrl instanceof String) {
            pushResult = pushMonitorData((String) pushUrl, clientId, pushData);
        } else {
            pushResult = new JSONObject();
            pushResult.put("message", "pushUrl is error!");
        }
        return pushResult;
    }

    static JSONObject pushMonitorData(String pushUrl, String clientId, JSONObject pushData) {
        JSONObject pushOutput = pushMonitorData(new String[]{pushUrl}, clientId, pushData);
        pushOutput.put("pushResult", pushOutput.getJSONArray("pushResult").get(0));
        return pushData;
    }

    static JSONObject pushMonitorData(String[] pushUrls, String clientId, JSONObject pushData) {

        JSONObject cpuPoint = buildCpuPoint();
        JSONObject memoryPoint = buildMemoryPoint();

        if (StrUtil.isNotBlank(clientId)) {
            pushData.put("clientId", clientId);
        }
        pushData.put("cpuPoint", cpuPoint);
        pushData.put("memoryPoint", memoryPoint);
        pushData.put("cpuTotal", getCpuTotal());
        String[] pushResult = new String[pushUrls.length];
        int i = 0;
        for (String url : pushUrls) {
            pushResult[i++] = HttpUtil.post(url, pushData);
        }

        JSONObject pushOutput = new JSONObject();
        pushOutput.put("pushData", pushData);
        pushOutput.put("pushResult", pushResult);
        return pushOutput;
    }

}
