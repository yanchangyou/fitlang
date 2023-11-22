package fit.lang.plugin.json.monitor;

import cn.hutool.core.util.StrUtil;
import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.HttpUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.parseHttpResult;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.setProxy;
import static fit.lang.plugin.json.monitor.JsonExecuteNodeMonitorUtil.*;
import static fit.lang.plugin.json.monitor.StartMonitorJsonExecuteNode.*;

/**
 * 推送监控数据
 */
public class PushClientMonitorDataJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        String clientId = parseStringField("clientId", input);
        JSONObject pushResult = pushMonitorData(nodeJsonDefine.get("pushUrl"), clientId, input.getData(), (JSONObject) nodeJsonDefine.get("pushProxy"));
        pushResult.put("pushUrl", nodeJsonDefine.get("pushUrl"));

        output.setData(pushResult);
    }

    public static JSONObject pushMonitorData(Object pushUrl, String clientId, JSONObject pushData, JSONObject pushProxy) {
        JSONObject pushResult;
        if (pushUrl instanceof JSONArray) {
            JSONArray pushUrls = (JSONArray) pushUrl;
            pushResult = pushMonitorData(pushUrls.toArray(new String[0]), clientId, pushData, pushProxy);
        } else if (pushUrl instanceof String) {
            pushResult = pushMonitorData((String) pushUrl, clientId, pushData, pushProxy);
        } else {
            pushResult = new JSONObject();
            pushResult.put("message", "pushUrl is error!");
        }
        return pushResult;
    }

    static JSONObject pushMonitorData(String pushUrl, String clientId, JSONObject pushData, JSONObject pushProxy) {
        JSONObject pushOutput = pushMonitorData(new String[]{pushUrl}, clientId, pushData, pushProxy);
        pushOutput.put("pushResult", pushOutput.getJSONArray("pushResult").get(0));
        return pushOutput;
    }

    static JSONObject pushMonitorData(String[] pushUrls, String clientId, JSONObject pushData, JSONObject pushProxy) {

        JSONObject cpuPoint = buildCpuPoint();
        JSONObject memoryPoint = buildMemoryPoint();

        if (StrUtil.isNotBlank(clientId)) {
            pushData.put("clientId", clientId);
        }
        pushData.put("cpuPoint", cpuPoint);
        pushData.put("memoryPoint", memoryPoint);
        pushData.put("cpuTotal", getCpuTotalShow());
        pushData.put("cpuCount", getCpuProcessorCount());
        pushData.put("memoryG", getMemoryG());
        String[] pushResult = new String[pushUrls.length];
        int i = 0;
        for (String url : pushUrls) {
            try {
                pushResult[i] = post(url, pushProxy, String.valueOf(pushData)).toJSONString();
            } catch (Exception e) {
                pushResult[i] = e.getMessage();
            }
            i++;
        }

        JSONObject pushOutput = new JSONObject();
        pushOutput.put("pushData", pushData);
        pushOutput.put("pushResult", pushResult);
        return pushOutput;
    }

    static JSONObject post(String url, JSONObject proxy, String body) {
        HttpRequest request = HttpUtil.createPost(url);

        setProxy(proxy, request);

        request.body(body);
        HttpResponse response = request.execute();

        return parseHttpResult(response);
    }

}
