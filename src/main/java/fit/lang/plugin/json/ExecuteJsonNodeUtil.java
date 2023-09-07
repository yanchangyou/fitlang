package fit.lang.plugin.json;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.HashMap;
import java.util.Map;

import static fit.lang.plugin.json.ExpressUtil.eval;
import static fit.lang.plugin.json.tool.ServerJsonExecuteNode.isWebNode;

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
     * 执行代码： 字符串
     *
     * @param input
     * @param flow
     * @return
     */
    public static String executeCode(String input, String flow) {
        return executeCode(input, flow, new JSONObject());
    }

    /**
     * 执行代码： 字符串
     *
     * @param input
     * @param flow
     * @return
     */
    public static String executeCode(String input, String flow, JSONObject contextParam) {
        if (StrUtil.isBlank(input)) {
            input = "{}";
        }
        JSONObject inputJson = JSONObject.parseObject(input);
        JSONObject flowDefine = JSONObject.parseObject(flow);
        return executeCode(inputJson, flowDefine, contextParam);
    }

    public static String executeCode(String input) {
        if (!isJsonText(input)) {
            throw new RuntimeException("input must be json, but found: ".concat(input));
        }
        input = ExecuteJsonNodeUtil.removeJsonComment(input);
        return executeCode(JSONObject.parseObject(input));
    }

    public static String executeCode(JSONObject input) {
        return executeCode(input, new JSONObject(), new JsonExecuteContext());
    }

    /**
     * 执行代码 json对象
     *
     * @param input
     * @param flow
     * @return
     */
    public static String executeCode(JSONObject input, JSONObject flow) {
        return executeCode(input, flow, new JSONObject());
    }

    public static String executeCode(JSONObject input, JSONObject flow, JSONObject contextParam) {
        return executeCode(input, flow, contextParam, new JsonExecuteContext());
    }

    public static String executeCode(JSONObject codeJson, JSONObject contextParam, JsonExecuteContext nodeContext) {

        JSONObject input;

        if (codeJson.containsKey("input")) {
            input = codeJson.getJSONObject("input");
        } else {
            input = new JSONObject(0);
        }
        return executeCode(input, codeJson, contextParam, nodeContext);
    }

    public static String executeCode(JSONObject input, JSONObject flow, JSONObject contextParam, JsonExecuteContext nodeContext) {

        JsonExecuteNodeOutput output = new JsonExecuteNodeOutput(nodeContext);
        JsonExecuteNodeInput nodeInput = new JsonExecuteNodeInput(nodeContext);
        nodeInput.setData(input);

        if (contextParam != null && !contextParam.isEmpty()) {
            nodeInput.getNodeContext().getAllAttribute().putAll(contextParam);
        }

        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(flow);
        executeNode.execute(nodeInput, output);

        String rawField = flow.getString("rawField");
        if (isWebNode(flow) && rawField != null) {
            Object returnValue = output.getData().get(rawField);
            return (returnValue == null) ? "" : returnValue.toString();
        }

        return output.getData().toJSONString();
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

    /**
     * 大致判断是否json字符串: TODO
     *
     * @param text
     * @return
     */
    public static boolean isJsonText(String text) {
        if (text == null) {
            return false;
        }
        text = text.trim();
        return text.startsWith("{") && text.endsWith("}");
    }

    /**
     * 去掉json注释
     *
     * @param text
     * @return
     */
    public static String removeJsonComment(String text) {
        if (!isJsonText(text)) {
            return text;
        }
        //去掉注释
        return text.trim().replaceAll("(\\r\\n|\\r|\\n)//.*", "");
    }

    /**
     * @param inputParamAndContextParam
     * @param nodeJsonDefine
     * @return
     */
    public static String parseNodeUrl(JSONObject inputParamAndContextParam, JSONObject nodeJsonDefine) {
        return parseNodeUrl(inputParamAndContextParam, nodeJsonDefine, "url");
    }

    /**
     * 从节点定义解析url ： 支持入参和配置，支持表达式
     *
     * @param inputParamAndContextParam
     * @param nodeJsonDefine
     * @return
     */
    public static String parseNodeUrl(JSONObject inputParamAndContextParam, JSONObject nodeJsonDefine, String urlFieldName) {
        String url = nodeJsonDefine.getString(urlFieldName);
        if (url == null) {
            return "";
        }
        url = (String) eval(url, inputParamAndContextParam);
        return url;
    }
}
