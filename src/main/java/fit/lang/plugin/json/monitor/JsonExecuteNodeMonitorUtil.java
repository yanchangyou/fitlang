package fit.lang.plugin.json.monitor;

import cn.hutool.core.math.MathUtil;
import cn.hutool.system.oshi.OshiUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import oshi.hardware.CentralProcessor;

import java.util.ArrayList;
import java.util.List;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.*;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.covertSecondToHour;

public class JsonExecuteNodeMonitorUtil {

    static String getCpuTotalShow() {
        CentralProcessor centralProcessor = OshiUtil.getHardware().getProcessor();
        return centralProcessor.getPhysicalProcessorCount() + " X " + covertToG(centralProcessor.getMaxFreq()) + "G";
    }

    static long getCpuTotal() {
        CentralProcessor centralProcessor = OshiUtil.getHardware().getProcessor();
        return centralProcessor.getPhysicalProcessorCount() * centralProcessor.getMaxFreq();
    }

    static int getCpuProcessorCount() {
        CentralProcessor centralProcessor = OshiUtil.getHardware().getProcessor();
        return centralProcessor.getPhysicalProcessorCount();
    }

    static long getMemoryG() {
        return OshiUtil.getHardware().getMemory().getTotal() / 1024 / 1024 / 1024;
    }

    public static List<JSONObject> fetchMonitorDataInLastSecond(JSONArray array, int second) {
        List<JSONObject> list = convertToList(array);
        return fetchMonitorDataInLastSecond(list, second);
    }

    public static List<JSONObject> fetchMonitorDataInLastSecond(List<JSONObject> list, int second) {
        List<JSONObject> result = fetchMonitorData(list, second);
        //限制返回数量，避免过大，太大就间隔采样
        return filterListByMaxLength(result, 512);
    }

    private static List<JSONObject> fetchMonitorData(List<JSONObject> list, int second) {
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
        return result;
    }

    /**
     * 汇总时间积分数据
     *
     * @param list
     * @param second
     * @return
     */
    public static JSONObject sumDataInLastSecond(List<JSONObject> list, int second, List<String> sumFields) {

        List<JSONObject> dataList = fetchMonitorData(list, second);

        JSONObject result = new JSONObject();
        if (dataList.isEmpty()) {
            return result;
        }

        for (String field : sumFields) {
            if (field.contains("timestamp")) {
                continue;
            }

            double sum = 0;
            JSONObject preItem = null;
            for (JSONObject item : dataList) {
                if (preItem == null) {
                    preItem = item;
                    continue;
                }

                long timestampDiff = (item.getLong("timestamp") - preItem.getLong("timestamp")) / 1000;
                double free = item.getDouble(field);
                sum += timestampDiff * free;

                preItem = item;
            }
            result.put(field, sum);
        }
        return result;
    }

    public static JSONObject sumCpuDataInLastSecond(JSONArray array, int second, int cpuCount) {
        List<JSONObject> list = convertToList(array);
        return sumCpuDataInLastSecond(list, second, cpuCount);
    }

    public static JSONObject sumCpuDataInLastSecond(List<JSONObject> list, int second, int cpuCount) {
        List<String> sumFields = new ArrayList<>();
        sumFields.add("free");
        sumFields.add("used");
        JSONObject sumResult = sumDataInLastSecond(list, second, sumFields);

        if (sumResult.isEmpty()) {
            return sumResult;
        }

        //获取的是百分比，需要转换
        double free = ExecuteJsonNodeUtil.round(sumResult.getDouble("free") / 100, 2);
        double used = ExecuteJsonNodeUtil.round(sumResult.getDouble("used") / 100, 2);

        sumResult.put("free", free);
        sumResult.put("used", used);
        sumResult.put("total", (free + used));
        double sumFree = ExecuteJsonNodeUtil.round(free / (free + used), 2);
        sumResult.put("sumFree", sumFree);
        sumResult.put("allCpuSumFree", ExecuteJsonNodeUtil.round(sumFree * cpuCount, 2));

        return sumResult;
    }

    public static JSONObject sumMemoryDataInLastSecond(JSONArray array, int second, long memoryG) {
        List<JSONObject> list = convertToList(array);
        return sumMemoryDataInLastSecond(list, second, memoryG);
    }

    public static JSONObject sumMemoryDataInLastSecond(List<JSONObject> list, int second, long memoryG) {
        List<String> sumFields = new ArrayList<>();
        sumFields.add("available");
        sumFields.add("used");
        JSONObject sumResult = sumDataInLastSecond(list, second, sumFields);

        if (sumResult.isEmpty()) {
            return sumResult;
        }

        double available = covertSecondToHour(sumResult.getDouble("available"));
        double used = covertSecondToHour(sumResult.getDouble("used"));

        sumResult.put("available", available);
        sumResult.put("used", used);
        sumResult.put("total", (available + used));
        double sumFree = ExecuteJsonNodeUtil.round(available / (available + used), 2);

        sumResult.put("sumFree", sumFree);
        sumResult.put("allMemorySumFree", ExecuteJsonNodeUtil.round(sumFree * memoryG, 2));

        return sumResult;
    }
}
