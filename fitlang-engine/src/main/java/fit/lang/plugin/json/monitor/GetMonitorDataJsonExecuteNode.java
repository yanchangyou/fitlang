package fit.lang.plugin.json.monitor;

import cn.hutool.core.date.DateUtil;
import cn.hutool.system.oshi.OshiUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import oshi.hardware.CentralProcessor;

import java.util.Date;

import static fit.lang.ExecuteNodeUtil.getNow;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.covertToG;
import static fit.lang.plugin.json.monitor.JsonExecuteNodeMonitorUtil.*;

/**
 * 获取监控信息
 */
public class GetMonitorDataJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        int second = parseIntField("second", input, 500);

        JSONObject result = new JSONObject();
        result.put("cpuPoints", StartMonitorJsonExecuteNode.getCpuGatherList(second));
        result.put("memoryPoints", StartMonitorJsonExecuteNode.getMemoryGatherList(second));
        CentralProcessor centralProcessor = OshiUtil.getHardware().getProcessor();
        result.put("cpuTotal", centralProcessor.getPhysicalProcessorCount() + " X " + covertToG(centralProcessor.getMaxFreq()) + "G");
        result.put("second", second);

        int cpuCount = JsonExecuteNodeMonitorUtil.getCpuProcessorCount();
        double memoryG = getMemoryG();
        result.put("cpuCount", cpuCount);
        result.put("memoryG", memoryG);

        result.put("sumCpuPoints", sumCpuDataInLastSecond(StartMonitorJsonExecuteNode.cpuGatherList, second, cpuCount));
        result.put("sumMemoryPoints", sumMemoryDataInLastSecond(StartMonitorJsonExecuteNode.memoryGatherList, second, memoryG));
        result.put("min", DateUtil.format(new Date(System.currentTimeMillis() - second * 1000L), "yyyy-MM-dd HH:mm:ss"));
        result.put("max", getNow());

        output.setData(result);
    }

}
