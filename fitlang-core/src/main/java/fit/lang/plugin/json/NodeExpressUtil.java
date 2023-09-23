package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;

import java.util.Map;

public class NodeExpressUtil {

    /**
     * json封装
     *
     * @param expressJson
     * @param param
     * @return
     */
    public static JSONObject eval(JSONObject expressJson, JSONObject param) {

        JSONObject result = new JSONObject();
        for (Map.Entry<String, Object> entry : expressJson.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();
            Object newValue;
            if (value instanceof JSONObject) {
                String itemResult = ExecuteJsonNodeUtil.executeCode(param, (JSONObject) value);
                newValue = JSONObject.parse(itemResult);
            } else if (value instanceof JSONArray) {
                JSONArray oldArray = (JSONArray) value;
                JSONArray newArray = new JSONArray(oldArray.size());
                for (Object item : oldArray) {
                    if (item instanceof JSONObject) {
                        String itemResult = ExecuteJsonNodeUtil.executeCode(param, (JSONObject) item);
                        newValue = JSONObject.parse(itemResult);
                        newArray.add(newValue);
                    } else {
                        throw new ExecuteNodeException("type must json object or array!");
                    }
                }
                newValue = newArray;
            } else {
                throw new ExecuteNodeException("type must json object or array!");
            }

            result.put(key, newValue);
        }
        return result;
    }
}