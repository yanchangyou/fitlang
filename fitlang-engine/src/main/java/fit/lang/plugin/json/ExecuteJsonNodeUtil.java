package fit.lang.plugin.json;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.crypto.SecureUtil;
import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.server.HttpServerRequest;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;

import java.io.File;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.SocketAddress;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static fit.lang.plugin.json.ExpressUtil.eval;
import static fit.lang.plugin.json.web.ServerJsonExecuteNode.isWebNode;

/**
 * 工具类
 */
public class ExecuteJsonNodeUtil {

    /**
     * 执行执行json
     *
     * @param inputJson
     * @return
     */
    public static JSONObject execute(JSONObject inputJson) {
        return JSONObject.parseObject(executeCode(inputJson));
    }

    /**
     * 执行执行json
     *
     * @param inputJson
     * @param node
     * @return
     */
    public static JSONObject execute(JSONObject inputJson, JsonExecuteNode node) {
        return JSONObject.parseObject(executeCode(inputJson, ((JSONObject) node.getNodeDefine().getData())));
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
        if (!isJsonObjectText(input)) {
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

        if (input == null || input.isEmpty()) {
            input = flow.getJSONObject("input");
        }
        if (input == null) {
            input = new JSONObject();
        }

        contextParam.putAll(input);
        input = eval(input, contextParam);

        //入参放入全局变量中
        nodeContext.setAttribute("input", input);
        nodeContext.setAttribute("nodeDefine", flow);

        JsonExecuteNodeOutput nodeOutput = new JsonExecuteNodeOutput(nodeContext);
        JsonExecuteNodeInput nodeInput = new JsonExecuteNodeInput(nodeContext);
        nodeInput.setData(input);

        if (contextParam != null && !contextParam.isEmpty()) {
            nodeContext.getAllAttribute().putAll(contextParam);
        }

        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(flow);
        executeNode.execute(nodeInput, nodeOutput);

        String rawField = flow.getString("rawField");
        if (isWebNode(flow) && rawField != null) {
            Object returnValue = nodeOutput.getData().get(rawField);
            return (returnValue == null) ? "" : returnValue.toString();
        }

        return nodeOutput.getData().toJSONString();
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
     * @param object
     * @return
     */
    public static boolean isJsonObjectText(Object object) {
        if (object == null) {
            return false;
        }
        if (!(object instanceof String)) {
            return false;
        }
        String text = (String) object;
        text = text.trim();
        return text.startsWith("{") && text.endsWith("}");
    }

    /**
     * 大致判断是否json字符串: TODO
     *
     * @param object
     * @return
     */
    public static boolean isJsonArrayText(Object object) {
        if (object == null) {
            return false;
        }
        if (!(object instanceof String)) {
            return false;
        }
        String text = (String) object;
        text = text.trim();
        return text.startsWith("[") && text.endsWith("]");
    }

    /**
     * 去掉json注释
     *
     * @param text
     * @return
     */
    public static String removeJsonComment(String text) {
        if (!isJsonObjectText(text)) {
            return text;
        }
        //去掉注释
        return text.trim().replaceAll("(\\r\\n|\\r|\\n)\\s*//.*", "");
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
        if (StrUtil.isBlank(url)) {
            url = inputParamAndContextParam.getString(urlFieldName);
            if (StrUtil.isBlank(url)) {
                return "";
            }
        }
        url = (String) eval(url, inputParamAndContextParam);
        return url;
    }

    /**
     * 读取节点定义文件 : 去掉注释
     *
     * @param serverFile
     * @return
     */
    public static String readNodeDefineFile(File serverFile) {
        return readNodeDefineFile(serverFile.getAbsolutePath());
    }

    /**
     * 读取节点定义文件 : 去掉注释
     *
     * @param serverFile
     * @return
     */
    public static String readNodeDefineFile(String serverFile) {
        return removeJsonComment(FileUtil.readUtf8String(serverFile));
    }

    /**
     * 是否本地IP： 127.0.0.1
     *
     * @param ip
     * @return
     */
    public static boolean isLocalIp(String ip) {
        return "localhost".equals(ip) || "127.0.0.1".equals(ip) || "0:0:0:0:0:0:0:1".equals(ip);
    }

    public static String getHttpClientIp(HttpServerRequest request) {
        return request.getClientIP();
    }

    /**
     * 解析http form格式的请求参数
     *
     * @param input
     * @param request
     * @param httpParam
     */
    public static void parseHttpFormParam(JsonExecuteNodeInput input, HttpRequest request, Object httpParam) {
        if (httpParam != null) {
            Object param = ExpressUtil.eval(httpParam, input.getInputParamAndContextParam());
            if (param instanceof JSONObject) {
                request.form((JSONObject) param);
            }
        } else {
            request.form(input.getData());
        }
    }

    /**
     * 设置http请求头
     *
     * @param header
     * @param request
     */
    public static void setHttpHeader(JSONObject header, HttpRequest request) {
        if (header != null && !header.isEmpty()) {
            request.addHeaders(toStringMap(header));
        }
    }

    /**
     * 设置代理
     *
     * @param proxyConfig
     * @param request
     */
    public static void setProxy(JSONObject proxyConfig, HttpRequest request) {

        if (proxyConfig != null && proxyConfig.containsKey("host") && proxyConfig.containsKey("port")) {

            String type = proxyConfig.getString("type");
            SocketAddress socketAddress = new InetSocketAddress(proxyConfig.getString("host"), proxyConfig.getInteger("port"));
            Proxy.Type typeEnum;
            if ("http".equals(type) || "HTTP".equals(type)) {
                typeEnum = Proxy.Type.HTTP;
            } else if ("socket".equals(type) || "socks".equals(type) || "SOCKS".equals(type)) {
                typeEnum = Proxy.Type.SOCKS;
            } else if ("direct".equals(type) || "DIRECT".equals(type)) {
                typeEnum = Proxy.Type.DIRECT;
            } else {
                typeEnum = Proxy.Type.HTTP;
            }
            request.setProxy(new Proxy(typeEnum, socketAddress));
        }
    }

    /**
     * 解析http结果
     *
     * @param response
     * @return
     */
    public static JSONObject parseHttpResult(HttpResponse response) {
        String responseText = response.body();

        if (responseText == null) {
            responseText = "";
        }
        JSONObject result = new JSONObject(1);
        if (isJsonObjectText(responseText)) {
            result = JSONObject.parseObject(responseText);
        } else if (isJsonArrayText(responseText)) {
            result.put("list", JSON.parseArray(responseText));
        } else {
            result.put("_raw", responseText);
        }
        return result;
    }

    /**
     * 构建路径， 支持相对路径和绝对路径
     *
     * @param path
     * @return
     */
    public static String buildFilePath(String path, String tips) {

        if (StrUtil.isBlank(path)) {
            throw new ExecuteNodeException(tips + ": file path is empty!");
        }

        // 绝对路径
        if (path.startsWith("/") || path.contains(":")) {
            // nothing
        } else { //相对路径
            path = ServerJsonExecuteNode.getServerFileDir() + "/" + path;
        }
        return path;
    }

    /**
     * 解析字段， 解析顺序：入参, 配置
     *
     * @param fieldName
     * @param input
     * @param nodeJsonDefine
     * @return
     */
    public static String parseStringField(String fieldName, JsonExecuteNodeInput input, JSONObject nodeJsonDefine) {
        String fieldValue = input.getString(fieldName);
        if (StrUtil.isBlank(fieldValue)) {
            fieldValue = nodeJsonDefine.getString(fieldName);
        }
        //尝试从上下文获取
        if (StrUtil.isBlank(fieldValue) && input.getNodeContext().getAttribute(fieldName) != null) {
            //TODO toString
            fieldValue = input.getNodeContext().getAttribute(fieldName).toString();
        }
        return (String) ExpressUtil.eval(fieldValue, input.getInputParamAndContextParam());
    }

    /**
     * 返回数组最大长度，超过长度就采样
     *
     * @param result
     * @param maxLength
     * @return
     */
    public static List<JSONObject> filterListByMaxLength(List<JSONObject> result, int maxLength) {
        if (result.size() > maxLength) {
            List<JSONObject> realResult = new ArrayList<>();
            int space = (result.size() + maxLength - 1) / maxLength;
            for (int i = 0; i < result.size(); i += space) {
                realResult.add(result.get(i));
            }
            result = realResult;
        }
        return result;
    }

    public static double round(double num, int precision) {
        double base = Math.pow(10, precision);
        return Math.round(base * num) / base;
    }

    public static double covertToG(long processorMaxFreq) {
        return Math.round(100.0 * processorMaxFreq / 1024 / 1024 / 1024) / 100.0;
    }

    public static double covertSecondToHour(double second) {
        return Math.round(100.0 * second / 3600) / 100.0;
    }

    public static JSONObject covertToG(JSONObject jsonObject) {
        for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
            Object value = entry.getValue();

            if (value instanceof Double) {
                value = Math.round(100.0 * ((double) entry.getValue() / 1024 / 1024 / 1024)) / 100.0;
            } else if (value instanceof Long) {
                value = Math.round(100.0 * ((long) entry.getValue() / 1024 / 1024 / 1024)) / 100.0;
            }
            entry.setValue(value);
        }
        return jsonObject;
    }

    public static JSONObject covertToLong(JSONObject jsonObject) {
        for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
            entry.setValue(Math.round(((Number) entry.getValue()).doubleValue()));
        }
        return jsonObject;
    }

    public static String buildInnerClientIp(String clientIp) {
        return SecureUtil.md5(clientIp);
    }

    public static String buildInnerClientId(String clientId, String clientIp) {
        return clientId + "." + clientIp;
    }

    public static List<JSONObject> convertToList(JSONArray array) {
        List<JSONObject> list = new ArrayList<>(array.size());
        for (Object item : array) {
            list.add((JSONObject) item);
        }
        return list;
    }

    /**
     * 路径拼接
     *
     * @param workspaceDir
     * @param path
     * @return
     */
    public static String joinFilePath(String workspaceDir, String path) {
        if(workspaceDir == null || path== null) {
            return null;
        }

        //避免遍历父目录
        path = path.replace("..", "");

        String filePath;
        //兼容收尾斜杠写法
        if (workspaceDir.endsWith("/") && path.startsWith("/")) {
            filePath = workspaceDir.concat(path.substring(1));
        } else if (!workspaceDir.endsWith("/") && !path.startsWith("/")) {
            filePath = workspaceDir.concat("/").concat(path);
        } else {
            filePath = workspaceDir.concat(path);
        }
        return filePath;
    }
}
