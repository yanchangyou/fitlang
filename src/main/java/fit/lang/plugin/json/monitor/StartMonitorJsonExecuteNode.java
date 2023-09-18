package fit.lang.plugin.json.monitor;

import cn.hutool.core.util.NumberUtil;
import cn.hutool.system.oshi.CpuInfo;
import cn.hutool.system.oshi.OshiUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import oshi.hardware.CentralProcessor;
import oshi.hardware.GlobalMemory;

import java.util.ArrayList;
import java.util.List;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.covertToG;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.filterListByMaxLength;
import static fit.lang.plugin.json.monitor.PushClientMonitorDataJsonExecuteNode.pushMonitorData;

/**
 * 简单监控器
 */
public class StartMonitorJsonExecuteNode extends JsonExecuteNode {

    /**
     * 全局变量
     */
    static List<JSONObject> cpuGatherList = new ArrayList<>();

    static List<JSONObject> memoryGatherList = new ArrayList<>();

    static Object pushUrl;

    static JSONObject pushProxy;

    static Thread thread;

    public static List<JSONObject> getGatherList(int second) {
        return getGatherList(cpuGatherList, second);
    }

    public static List<JSONObject> getMemoryGatherList(int second) {
        return getGatherList(memoryGatherList, second);
    }

    public static List<JSONObject> getGatherList(JSONArray array, int second) {
        List<JSONObject> list = new ArrayList<>(array.size());
        for (Object item : array) {
            list.add((JSONObject) item);
        }
        return getGatherList(list, second);
    }

    public static List<JSONObject> getGatherList(List<JSONObject> list, int second) {
        if (second < 0) {
            second = 0;
        }
        List<JSONObject> result = new ArrayList<>();
        long millisecond = System.currentTimeMillis() - second * 1000L;
        for (JSONObject row : list) {
            if (row.getLong("timestamp") != null && row.getLong("timestamp") > millisecond) {
                result.add(row);
            }
        }
        //限制返回数量，避免过大，太大就间隔采样
        return filterListByMaxLength(result, 512);
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject result = new JSONObject();
        if (cpuGatherList == null) {//避免重复启动
            cpuGatherList = new ArrayList<>();
        }

        String secondText = parseStringField("second", input);

        pushUrl = nodeJsonDefine.get("pushUrl");
        pushProxy = nodeJsonDefine.getJSONObject("pushProxy");

        int second = 5;
        if (NumberUtil.isInteger(secondText)) {
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
                        Thread.sleep(second * 1000L - 1000L);
                    } catch (InterruptedException e) {
                        System.out.println("stop thread" + this.getName());
                        break;
                    }
                    try {

                        JSONObject cpuPoint = buildCpuPoint();
                        cpuGatherList.add(cpuPoint);
                        JSONObject memoryPoint = buildMemoryPoint();
                        memoryGatherList.add(memoryPoint);

                        if (pushUrl instanceof JSONArray || pushUrl instanceof String) {
                            pushMonitorData(pushUrl, null, new JSONObject(), pushProxy);
                        }
                    } catch (Exception e) {
                        System.out.println("fit-monitor-error: " + e.getMessage());
                    }
                }
            }
        };
    }

    static String getCpuTotal() {
        CentralProcessor centralProcessor = OshiUtil.getHardware().getProcessor();
        return centralProcessor.getPhysicalProcessorCount() + " X " + covertToG(centralProcessor.getMaxFreq()) + "G";
    }

    static JSONObject buildCpuPoint() {
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
        return current;
    }

    static JSONObject buildMemoryPoint() {
        JSONObject current = new JSONObject();
        GlobalMemory globalMemory = OshiUtil.getMemory();
        current.put("total", covertToG(globalMemory.getTotal()));
        current.put("available", covertToG(globalMemory.getAvailable()));
        current.put("used", covertToG(globalMemory.getTotal() - globalMemory.getAvailable()));
        current.put("timestamp", System.currentTimeMillis());
        current.put("timestampShow", ExecuteNodeUtil.getNow());
        return current;
    }
}
