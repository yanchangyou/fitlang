package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.HashMap;
import java.util.Map;

/**
 * 工具类
 */
public class ExecuteJsonNodeUtil {

    /**
     * 执行执行json
     *
     * @param inputJson
     * @param node
     * @return
     */
    public static JSONObject execute(JSONObject inputJson, JsonExecuteNode node) {

        JsonExecuteContext nodeContext = new JsonExecuteContext();

        JsonExecuteNodeInput input = new JsonExecuteNodeInput(nodeContext);

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);

        input.setData(inputJson);

        node.execute(input, output);

        return output.getData();
    }

    /**
     * json to string map
     *
     * @param jsonObject
     * @return
     */
    public static Map<String, String> toStringMap(JSONObject jsonObject) {
        if (jsonObject == null) {
            return null;
        }
        Map<String, String> map = new HashMap<>();
        for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
            map.put(entry.getKey(), entry.getValue() == null ? null : entry.getValue().toString());
        }
        return map;
    }

}
