package fit.lang.plugin.json.monitor;

import cn.hutool.core.util.NumberUtil;
import cn.hutool.system.oshi.CpuInfo;
import cn.hutool.system.oshi.OshiUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.ArrayList;
import java.util.List;

/**
 * 简单监控器
 */
public class StartMonitorJsonExecuteNode extends JsonExecuteNode {

    /**
     * 全局变量
     */
    static List<JSONObject> cpuGatherList;
    static Thread thread;

    public static List<JSONObject> getCpuGatherList() {
        return cpuGatherList;
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
                setName("git-monitor-" + System.currentTimeMillis());
                while (true) {
                    try {
                        Thread.sleep(second * 1000L - 1000);
                    } catch (InterruptedException e) {
                        System.out.println("stop thread" + this.getName());
                        break;
                    }
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
            }
        };
    }
}
