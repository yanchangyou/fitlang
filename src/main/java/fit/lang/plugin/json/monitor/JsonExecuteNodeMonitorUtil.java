package fit.lang.plugin.json.monitor;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.filterListByMaxLength;

public class JsonExecuteNodeMonitorUtil {

    public static List<JSONObject> fetchMonitorDataInLastSecond(JSONArray array, int second) {
        List<JSONObject> list = new ArrayList<>(array.size());
        for (Object item : array) {
            list.add((JSONObject) item);
        }
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
    public static JSONObject sumDataInLastSecond(List<JSONObject> list, int second) {

        List<JSONObject> dataList = fetchMonitorData(list, second);

        JSONObject result = new JSONObject();
        if (dataList.isEmpty()) {
            return result;
        }

        JSONObject first = list.get(0);
        Set<String> fields = first.keySet();

        for (String field : fields) {
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

                long timestampDiff = item.getLong("timestamp") - preItem.getLong("timestamp");
                double free = item.getDouble(field);
                sum += timestampDiff * free;

                preItem = item;
            }
            result.put(field, sum);
        }
        return result;
    }

}
