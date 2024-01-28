package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import org.mvel2.MVEL;

import java.util.Map;

public class ExpressUtil {

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

        if (!param.containsKey("_FitLang")) {
            param.put("_FitLang", FitLangExpressTool.INSTANCE);
        }
        if (!param.containsKey("FitLang")) {
            param.put("FitLang", FitLangExpressTool.INSTANCE);
        }

        return MVEL.eval(realExpress, param, param);
    }

    /**
     * json封装
     *
     * @param expressJson
     * @param param
     * @return
     */
    public static JSONObject eval(JSONObject expressJson, JSONObject param) {
        return (JSONObject) eval((Object) expressJson, param);
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
                newArray.add(newItem);
            }
            newValue = newArray;
        }
        return newValue;
    }
}