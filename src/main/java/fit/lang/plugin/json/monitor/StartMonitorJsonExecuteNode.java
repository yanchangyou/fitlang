package fit.lang.plugin.json.monitor;

import cn.hutool.core.util.NumberUtil;
import cn.hutool.system.oshi.CpuInfo;
import cn.hutool.system.oshi.OshiUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import oshi.hardware.GlobalMemory;

import java.util.ArrayList;
import java.util.List;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.covertToG;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.filterListByMaxLength;

/**
 * 简单监控器
 */
public class StartMonitorJsonExecuteNode extends JsonExecuteNode {

    /**
     * 全局变量
     */
    static List<JSONObject> cpuGatherList = new ArrayList<>();

    static List<JSONObject> memoryGatherList = new ArrayList<>();

    static Thread thread;

    public static List<JSONObject> getCpuGatherList(int second) {
        return getCpuGatherList(cpuGatherList, second);
    }

    public static List<JSONObject> getMemoryGatherList(int second) {
        return getCpuGatherList(memoryGatherList, second);
    }

    public static List<JSONObject> getCpuGatherList(List<JSONObject> list, int second) {
        if (second < 0) {
            second = 0;
        }
        List<JSONObject> result = new ArrayList<>();
        long millisecond = System.currentTimeMillis() - second * 1000L;
        for (JSONObject row : list) {
            if (row.getLong("timestamp") > millisecond) {
                result.add(row);
            }
        }
        //最多返回500条，避免过大，太大就间隔采样
        return filterListByMaxLength(result, 500);
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject result = new JSONObject();
        if (cpuGatherList == null) {//避免重复启动
            cpuGatherList = new ArrayList<>();
        }

        String secondText = parseStringField("second", input);

        int second = 5;
        if (secondText != null && NumberUtil.isInteger(secondText)) {
            second = Integer.parseInt(secondText);
        }
        //至少1秒
        if (second < 1) {
            second = 1;
        }

        if (thread != null && thread.isAlive()) {
            thread.interrupt();
        }

        thread = createMonitorThread(second);
        thread.start();

        result.put("message", "start monitor OK");
        result.put("second", second);

        output.setData(result);
    }

    private static Thread createMonitorThread(int second) {
        return new Thread() {
            @Override
            public void run() {
                setName("fit-monitor-" + System.currentTimeMillis());
                while (true) {
                    try {
                        Thread.sleep(second * 1000L - 1000);
                    } catch (InterruptedException e) {
                        System.out.println("stop thread" + this.getName());
                        break;
                    }
                    addCpuPoint();
                    addMemoryPoint();
                }
            }
        };
    }

    private static void addCpuPoint() {
        JSONObject current = new JSONObject();
        CpuInfo cpuInfo = OshiUtil.getCpuInfo();
        current.put("total", cpuInfo.getToTal());
        current.put("free", cpuInfo.getFree());
        current.put("sys", cpuInfo.getSys());
        current.put("used", cpuInfo.getUsed());
        current.put("user", cpuInfo.getUser());
        current.put("wait", cpuInfo.getWait());
        current.put("timestamp", System.currentTimeMillis());
        current.put("timestampShow", ExecuteNodeUtil.getNow());
        cpuGatherList.add(current);
    }

    private static void addMemoryPoint() {
        JSONObject current = new JSONObject();
        GlobalMemory globalMemory = OshiUtil.getMemory();
        current.put("total", covertToG(globalMemory.getTotal()));
        current.put("available", covertToG(globalMemory.getAvailable()));
        current.put("used", covertToG(globalMemory.getTotal() - globalMemory.getAvailable()));
        current.put("timestamp", System.currentTimeMillis());
        current.put("timestampShow", ExecuteNodeUtil.getNow());
        memoryGatherList.add(current);
    }
}
