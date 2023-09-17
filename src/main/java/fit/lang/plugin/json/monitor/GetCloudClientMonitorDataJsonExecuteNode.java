package fit.lang.plugin.json.monitor;

import cn.hutool.core.util.StrUtil;
import cn.hutool.system.oshi.OshiUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.cloud.CloudServerJsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import oshi.hardware.CentralProcessor;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.covertToG;
import static fit.lang.plugin.json.monitor.StartMonitorJsonExecuteNode.getGatherList;

/**
 * 获取cloud场景下，获取client推送的监控信息
 */
public class GetCloudClientMonitorDataJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        int second = parseIntField("second", input, 500);

        JSONObject result = new JSONObject();
        CentralProcessor centralProcessor = OshiUtil.getHardware().getProcessor();
        result.put("cpuTotal", centralProcessor.getPhysicalProcessorCount() + " X " + covertToG(centralProcessor.getMaxFreq()) + "G");

        String clientId = parseStringField("clientId", input);

        result.put("clientId", clientId);

        if (StrUtil.isNotBlank(clientId)) {
            JSONObject session = CloudServerJsonExecuteNode.getSession(clientId);
            if (session == null) {
                result.put("message", "clientId not existed!");
            } else {
                JSONObject monitor = session.getJSONObject("info").getJSONObject("monitor");
                if (monitor != null) {
                    result.put("cpuPoints", getGatherList(monitor.getJSONArray("cpuPoints"), second));
                    result.put("memoryPoints", getGatherList(monitor.getJSONArray("memoryPoints"), second));
                } else {
                    result.put("message", "client monitor data is empty, need client push data!");
                }
            }
        } else {
            result.put("message", "clientId is required!");
        }

        output.setData(result);
    }
}
