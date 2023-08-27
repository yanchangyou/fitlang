package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;

import java.util.Map;

public class ExpressUtil {

    private static final AviatorEvaluatorInstance engine = AviatorEvaluator.getInstance();

    /**
     * 表达式计算
     *
     * @param express
     * @param param
     * @return
     */
    public static Object eval(String express, JSONObject param) {
        if (express == null) {
            return null;
        }

        express = express.trim();
        if (!express.startsWith("${") || !express.endsWith("}")) {
            return express;
        }

        String realExpress = express.substring(2, express.length() - 1);

        return engine.execute(realExpress, param);
    }

    /**
     * 表达式计算
     *
     * @param value
     * @param param
     * @return
     */
    public static Object eval(Object value, JSONObject param) {
        if (value == null) {
            return null;
        }

        Object newValue = value;
        if (value instanceof String) {
            newValue = eval((String) value, param);
        } else if (value instanceof JSONObject) {
            JSONObject newJson = new JSONObject();
            for (Map.Entry<String, Object> entry : ((JSONObject) value).entrySet()) {
                String key = entry.getKey();
                Object itemValue = entry.getValue();
                Object newItemValue = eval(itemValue, param);
                newJson.put(key, newItemValue);
            }
            newValue = newJson;
        } else if (value instanceof JSONArray) {
            JSONArray oldArray = (JSONArray) value;
            JSONArray newArray = new JSONArray(oldArray.size());
            for (Object item : oldArray) {
                Object newItem = eval(item, param);
                ;
                newArray.add(newItem);
            }
            newValue = newArray;
        }
        return newValue;
    }
}