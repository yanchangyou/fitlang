package fit.lang.plugin.json.info;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import oshi.SystemInfo;
import oshi.hardware.*;
import oshi.software.os.OperatingSystem;

import java.util.List;

/**
 * 获取系统基本信息：依赖oshi实现
 */
public class SystemBaseInfoJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONArray shardFields = nodeJsonDefine.getJSONArray("shareFields");
        if (shardFields == null) {
            return;
        }

        JSONObject systemInfoJson = new JSONObject();

        SystemInfo systemInfo = new SystemInfo();

        HardwareAbstractionLayer hardware = systemInfo.getHardware();

        //计算机
        ComputerSystem computerSystem = hardware.getComputerSystem();

        if (shardFields.contains("computerManufacturer")) {
            String computerManufacturer = computerSystem.getManufacturer();//制造商
            systemInfoJson.put("computerManufacturer", computerManufacturer);
        }
        if (shardFields.contains("computerModel")) {
            String computerModel = computerSystem.getModel();//型号
            systemInfoJson.put("computerModel", computerModel);
        }

        //CPU
        CentralProcessor processor = hardware.getProcessor();
        if (shardFields.contains("processorName")) {
            String processorName = processor.getProcessorIdentifier().getName();//处理名称
            systemInfoJson.put("processorName", processorName);
        }
        if (shardFields.contains("processorPhysicalCount")) {
            int physicalProcessorCount = processor.getPhysicalProcessorCount();//物理核心数
            systemInfoJson.put("processorPhysicalCount", physicalProcessorCount);
        }
        if (shardFields.contains("processorLogicalCount")) {
            int logicalProcessorCount = processor.getLogicalProcessorCount();//逻辑核心数
            systemInfoJson.put("processorLogicalCount", logicalProcessorCount);
        }

        if (shardFields.contains("processorMaxFreq")) {
            long processorMaxFreq = processor.getMaxFreq();
            systemInfoJson.put("processorMaxFreq", covertToG(processorMaxFreq) + "G");
        }

        //内存
        GlobalMemory memory = hardware.getMemory();
        if (shardFields.contains("memoryTotal")) {
            long memoryTotal = memory.getTotal();//内存大小
            systemInfoJson.put("memoryTotal", covertToG(memoryTotal) + "G");
        }
        if (shardFields.contains("memoryAvailable")) {
            long memoryAvailable = memory.getAvailable();
            systemInfoJson.put("memoryAvailable", covertToG(memoryAvailable) + "G");
        }

        OperatingSystem os = systemInfo.getOperatingSystem();
        if (shardFields.contains("osManufacturer")) {
            String osManufacturer = os.getManufacturer();
            systemInfoJson.put("osManufacturer", osManufacturer);
        }

        if (shardFields.contains("osFamily")) {
            String osFamily = os.getFamily();
            systemInfoJson.put("osFamily", osFamily);
        }

        if (shardFields.contains("osVersion")) {
            OperatingSystem.OSVersionInfo osVersion = os.getVersionInfo();
            systemInfoJson.put("osVersion", osVersion.getVersion());
        }

        if (shardFields.contains("osBit")) {
            int osBit = os.getBitness();
            systemInfoJson.put("osBit", osBit);
        }

        output.setData(systemInfoJson);

    }

    private static double covertToG(long processorMaxFreq) {
        return Math.round(10.0 * processorMaxFreq / 1024 / 1024 / 1024) / 10.0;
    }
}
