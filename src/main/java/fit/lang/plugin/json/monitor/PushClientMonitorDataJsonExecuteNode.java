package fit.lang.plugin.json.monitor;

import cn.hutool.core.util.StrUtil;
import cn.hutool.http.HttpUtil;
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
        String pushUrl = parseStringField("pushUrl", input);

        JSONObject pushData = pushMonitorData(pushUrl, clientId, input.getData());

        output.setData(pushData);
    }

    static JSONObject pushMonitorData(String pushUrl, String clientId, JSONObject pushData) {

        JSONObject cpuPoint = buildCpuPoint();
        JSONObject memoryPoint = buildMemoryPoint();

        if (StrUtil.isNotBlank(clientId)) {
            pushData.put("clientId", clientId);
        }
        pushData.put("cpuPoint", cpuPoint);
        pushData.put("memoryPoint", memoryPoint);
        pushData.put("cpuTotal", getCpuTotal());

        String pushResult = HttpUtil.post(pushUrl, pushData);

        JSONObject pushOutput = new JSONObject();
        pushOutput.put("pushData", pushData);
        pushOutput.put("pushResult", pushResult);
        return pushOutput;
    }

}
