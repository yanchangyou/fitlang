package fit.lang.plugin.json.info;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import oshi.SystemInfo;
import oshi.hardware.*;

import java.util.List;

/**
 * 获取系统基本信息：依赖oshi实现
 */
public class SystemBaseInfoJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject systemInfoJson = new JSONObject();

        SystemInfo systemInfo = new SystemInfo();

        HardwareAbstractionLayer hardware = systemInfo.getHardware();

        //计算机
        ComputerSystem computerSystem = hardware.getComputerSystem();
        String computerManufacturer = computerSystem.getManufacturer();//制造商
        String computerModel = computerSystem.getModel();//型号

        systemInfoJson.put("computerManufacturer", computerManufacturer);
        systemInfoJson.put("computerModel", computerModel);

        //CPU
        CentralProcessor processor = hardware.getProcessor();
        String processorName = processor.getProcessorIdentifier().getName();//处理名称
        int physicalProcessorCount = processor.getPhysicalProcessorCount();//物理核心数
        int logicalProcessorCount = processor.getLogicalProcessorCount();//逻辑核心数
        long processorMaxFreq = processor.getMaxFreq();

        systemInfoJson.put("processorName", processorName);
        systemInfoJson.put("physicalProcessorCount", physicalProcessorCount);
        systemInfoJson.put("logicalProcessorCount", logicalProcessorCount);
        systemInfoJson.put("processorMaxFreq", Math.round(10.0 * processorMaxFreq / 1024 / 1024 / 1024) / 10.0 + "G");

        //内存
        GlobalMemory memory = hardware.getMemory();
        long memoryTotal = memory.getTotal();//内存大小
        long memoryAvailable = memory.getAvailable();

        systemInfoJson.put("memoryTotal", (memoryTotal / 1024 / 1024 / 1024) + "G");
        systemInfoJson.put("memoryAvailable", (memoryAvailable / 1024 / 1024 / 1024) + "G");

        //硬盘
        List<HWDiskStore> diskStoreList = hardware.getDiskStores();

        JSONArray diskArray = new JSONArray();
        for (HWDiskStore store : diskStoreList) {
            JSONObject disk = new JSONObject();
            disk.put("model", store.getModel());
            disk.put("name", store.getName());
            disk.put("size", (store.getSize() / 1024 / 1024 / 1024) + "G");
            diskArray.add(disk);
        }

        systemInfoJson.put("disks", diskArray);

        output.setData(systemInfoJson);

    }
}
