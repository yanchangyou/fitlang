package fit.lang.plugin.json;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.crypto.SecureUtil;
import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.server.HttpServerRequest;
import cn.hutool.system.SystemUtil;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import com.alibaba.fastjson2.schema.JSONSchema;
import fit.lang.ExecuteNodeException;
import fit.lang.define.ExecuteNode;
import fit.lang.define.ExecuteNodeData;
import fit.lang.plugin.json.define.*;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;

import java.io.File;
import java.math.BigInteger;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.SocketAddress;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.*;

import static fit.lang.ExecuteNodeUtil.getUserHome;
import static fit.lang.plugin.json.ExecuteJsonNodeConst.FIELD_NAME_OF_IDEA_PROJECT;
import static fit.lang.plugin.json.ExpressUtil.eval;

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
        return executeCode(input, new JSONObject());
    }

    public static String executeCode(String input, JSONObject contextParam) {
        if (!isJsonObjectText(input)) {
            throw new RuntimeException("input must be json, but found: ".concat(input));
        }
        input = ExecuteJsonNodeUtil.removeJsonComment(input);
        return executeCode(new JSONObject(), JSONObject.parseObject(input), contextParam);
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
        if (contextParam == null) {
            contextParam = new JSONObject();
        }

        contextParam.putAll(input);
        input = eval(input, contextParam);

        //入参放入全局变量中
        nodeContext.setAttribute("input", input);
        nodeContext.setAttribute("nodeDefine", flow);

        JsonExecuteNodeOutput nodeOutput = new JsonExecuteNodeOutput(nodeContext);
        JsonExecuteNodeInput nodeInput = new JsonExecuteNodeInput(nodeContext);
        nodeInput.setData(input);

        if (!contextParam.isEmpty()) {
            nodeContext.putAllAttribute(contextParam);
        }

        ExecuteNode executeNode = new JsonDynamicFlowExecuteEngine(flow);

        executeNode.setNodeContext(nodeContext);

        executeNode.execute(nodeInput, nodeOutput);

        String outputRawField = flow.getString("outputRawField");
        if (StrUtil.isNotBlank(outputRawField)) {
            Object returnValue = nodeOutput.getData().get(outputRawField);
            return (returnValue == null) ? "" : returnValue.toString();
        }

        JSONObject result = nodeOutput.getData();
        if (Boolean.TRUE.equals(flow.getBoolean("debug"))) {
            Map<String, Object> context = nodeInput.getNodeContext().getAllAttribute();
            context.remove(FIELD_NAME_OF_IDEA_PROJECT);
            result = nodeOutput.getData().clone();
            result.put("_debug", context);
        }
        return toJsonText(result);
    }

    public static Map<String, String> toStringMap(JSONObject jsonObject) {
        return toStringMap(jsonObject, false);
    }

    /**
     * json to string map
     *
     * @param jsonObject
     * @return
     */
    public static Map<String, String> toStringMap(JSONObject jsonObject, boolean needRemoveEmpty) {
        return toStringMap(jsonObject, false, false);
    }

    /**
     * json to string map
     *
     * @param jsonObject
     * @return
     */
    public static Map<String, String> toStringMapForCookie(JSONObject jsonObject) {
        return toStringMap(jsonObject, false, true);
    }

    /**
     * json to string map
     *
     * @param jsonObject
     * @return
     */
    public static Map<String, String> toStringMap(JSONObject jsonObject, boolean needRemoveEmpty, boolean isCookieJoin) {
        if (jsonObject == null) {
            return null;
        }
        Map<String, String> map = new HashMap<>();
        for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
            String value;
            Object valueObject = entry.getValue();
            StringBuilder builder = new StringBuilder();
            if (isCookieJoin && valueObject instanceof JSONObject) {
                Map<String, String> itemMap = toStringMap((JSONObject) valueObject);
                for (Map.Entry<String, String> itemEntry : itemMap.entrySet()) {
                    builder.append(itemEntry.getKey()).append("=").append(itemEntry.getValue()).append(";");
                }
                value = builder.toString();
            } else {
                value = entry.getValue() == null ? null : entry.getValue().toString();
            }
            if (needRemoveEmpty && StrUtil.isBlank(value)) {
                continue;
            }
            map.put(entry.getKey(), value);
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
     * @param useInput
     */
    public static Object parseHttpFormParam(JsonExecuteNodeInput input, HttpRequest request, Object httpParam, boolean useInput) {
        Object param = ExpressUtil.eval(httpParam, input.getInputParamAndContextParam());
        if (param instanceof JSONObject) {
            if (useInput) {
                JSONObject inputParam = input.getData().clone();
                inputParam.putAll((JSONObject) param);
                param = inputParam;
            }
            request.form((JSONObject) param);
            return param;
        }
        return null;
    }

    /**
     * 设置http请求头
     *
     * @param header
     * @param request
     */
    public static void setHttpHeader(JSONObject header, HttpRequest request) {
        if (header != null && !header.isEmpty()) {
            request.addHeaders(toStringMapForCookie(header));
        }
    }

    /**
     * 设置代理
     *
     * @param proxyConfig
     * @param request
     */
    public static void setProxy(JSONObject proxyConfig, HttpRequest request) {

        Proxy proxy = buildProxy(proxyConfig);
        if (proxy != null) {
            request.setProxy(proxy);
        }
    }

    /**
     * 构建proxy
     *
     * @param proxyConfig
     * @return
     */
    public static Proxy buildProxy(JSONObject proxyConfig) {

        if (proxyConfig == null || StrUtil.isBlank(proxyConfig.getString("host")) || !proxyConfig.containsKey("port")) {
            return null;
        }
        String type = proxyConfig.getString("type");
        SocketAddress socketAddress = new InetSocketAddress(proxyConfig.getString("host"), proxyConfig.getInteger("port"));
        Proxy.Type typeEnum;
        if ("http".equals(type) || "HTTP".equals(type)) {
            typeEnum = Proxy.Type.HTTP;
        } else if ("socket".equalsIgnoreCase(type) || "socks".equalsIgnoreCase(type)) {
            typeEnum = Proxy.Type.SOCKS;
        } else if ("direct".equals(type) || "DIRECT".equals(type)) {
            typeEnum = Proxy.Type.DIRECT;
        } else {
            typeEnum = Proxy.Type.HTTP;
        }
        return new Proxy(typeEnum, socketAddress);
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
        if (!response.isOk()) {
            result.put("httpStatus", response.getStatus());
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
        Object object = parseField(fieldName, input, nodeJsonDefine);
        if (object == null) {
            return null;
        }
        return object.toString();
    }

    /**
     * 解析字段， 解析顺序：入参, 配置
     *
     * @param fieldName
     * @param input
     * @param nodeJsonDefine
     * @return
     */
    public static Object parseField(String fieldName, JsonExecuteNodeInput input, JSONObject nodeJsonDefine) {
        Object fieldValue = input.get(fieldName);
        if (fieldValue == null) {
            fieldValue = nodeJsonDefine.get(fieldName);
        }
        //尝试从上下文获取
        if (fieldValue == null && input.getNodeContext().getAttribute(fieldName) != null) {
            //TODO toString
            fieldValue = input.getNodeContext().getAttribute(fieldName);
        }
        JSONObject inputAndContextParam = input.getInputParamAndContextParam();

        //把节点定义也放入解析
        for (Map.Entry<String, Object> define : nodeJsonDefine.entrySet()) {
            inputAndContextParam.putIfAbsent(define.getKey(), define.getValue());
        }
        return ExpressUtil.eval(fieldValue, inputAndContextParam);
    }

    /**
     * 判断是否数组字段
     *
     * @param fieldName
     * @param input
     * @param nodeJsonDefine
     * @return
     */
    public static boolean isArrayField(String fieldName, JsonExecuteNodeInput input, JSONObject nodeJsonDefine) {

        Object value = input.get(fieldName);
        if (value == null) {
            value = nodeJsonDefine.get(fieldName);
        }
        return value != null && value instanceof JSONArray;
    }

    /**
     * 解析字符串数组
     *
     * @param fieldName
     * @param input
     * @param nodeJsonDefine
     * @return
     */
    public static List<String> parseStringArray(String fieldName, JsonExecuteNodeInput input, JSONObject nodeJsonDefine) {

        Object value = input.get(fieldName);
        if (value == null) {
            value = nodeJsonDefine.get(fieldName);
        }
        if (value == null) {
            return null;
        }
        JSONArray array = null;
        if (value instanceof JSONArray) {
            array = (JSONArray) value;
        } else {
            array = new JSONArray(1);
            array.add(value);
        }

        List<String> result = new ArrayList<>(array.size());
        for (Object item : array) {
            if (item == null) {
                continue;
            }
            String line = item.toString();
            String parseValue = (String) ExpressUtil.eval(line, input.getInputParamAndContextParam());
            result.add(parseValue);
        }
        return result;
    }

    /**
     * 解析字段值
     *
     * @param fieldValue
     * @param input
     * @return
     */
    public static String parseStringExcludeContext(String fieldValue, JsonExecuteNodeInput input) {
        Object value = ExpressUtil.eval(fieldValue, input.getData());
        return value == null ? "" : value.toString();
    }

    /**
     * 解析字段值
     *
     * @param fieldValue
     * @param input
     * @return
     */
    public static String parseString(String fieldValue, JsonExecuteNodeInput input) {
        //尝试从上下文获取
        if (StrUtil.isBlank(fieldValue) && input.getNodeContext().getAttribute(fieldValue) != null) {
            //TODO toString
            fieldValue = input.getNodeContext().getAttribute(fieldValue).toString();
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
        if (workspaceDir == null || path == null) {
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

    /**
     * 转换为json 文本
     *
     * @param jsonObject
     * @return
     */
    public static String toJsonText(JSONObject jsonObject) {
        return jsonObject.toJSONString(JSONWriter.Feature.WriteMapNullValue);
    }

    /**
     * 转换为json 文本
     *
     * @param jsonArray
     * @return
     */
    public static String toJsonText(JSONArray jsonArray) {
        return jsonArray.toJSONString(JSONWriter.Feature.WriteMapNullValue);
    }

    /**
     * 转换为json 文本
     *
     * @param jsonObject
     * @return
     */
    public static String toJsonTextWithFormat(JSONObject jsonObject) {
        return jsonObject.toJSONString(JSONWriter.Feature.WriteMapNullValue, JSONWriter.Feature.PrettyFormat)
                .replaceAll("\\t", "    ")
                .replace("\":\"", "\": \"")
                .replace("\":{\"", "\": {")
                .replace("\":true", "\": true")
                .replace("\":false", "\": false")
                .replace("\":null", "\": null")
                ;
    }

    /**
     * 转换为json 文本
     *
     * @param jsonArray
     * @return
     */
    public static String toJsonTextWithFormat(JSONArray jsonArray) {
        return jsonArray.toJSONString(JSONWriter.Feature.WriteMapNullValue, JSONWriter.Feature.PrettyFormat)
                .replaceAll("\\t", "    ")
                .replace("\":\"", "\": \"")
                .replace("\":true", "\": true")
                .replace("\":false", "\": false")
                .replace("\":null", "\": null")
                ;
    }

    /**
     * 读取配置字段:兼容数组和非数组
     *
     * @param jsonNodeDefine
     * @param fieldName
     * @return
     */
    public static JSONArray getConfigFields(JSONObject jsonNodeDefine, String fieldName) {
        Object field = jsonNodeDefine.get(fieldName);
        JSONArray fields = new JSONArray();
        if (field instanceof JSONArray) {
            fields = (JSONArray) field;
        } else if (field != null) {
            fields.add(field);
        }
        return fields;
    }

    /**
     * 获取json node define
     *
     * @param nodeDefine
     * @return
     */
    public static JSONObject getJsonData(ExecuteNodeData nodeDefine) {
        return ((JsonExecuteNodeData) nodeDefine).getData();
    }

    public static boolean isInServerEnvironment() {
        return !Thread.currentThread().getContextClassLoader().toString().contains("com.intellij.");
    }

    public static int getUrlPort(URL httpUrl) {
        return httpUrl.getPort() == -1 ? httpUrl.getDefaultPort() : httpUrl.getPort();
    }

    /**
     * 操作系统字符集
     *
     * @return
     */
    public static Charset getSystemCharset() {
        return Charset.forName(SystemUtil.getProps().getProperty("sun.jnu.encoding", "UTF-8"));
    }

    /**
     * 获取文件字符集
     *
     * @return
     */
    public static Charset getFileCharset() {
        return Charset.forName(SystemUtil.getProps().getProperty("file.encoding", "UTF-8"));
    }


    /**
     * 构建上下文参数
     *
     * @param projectPath
     * @param file
     * @return
     */
    public static JSONObject buildContextParam(String projectPath, File file) {
        JSONObject contextParam = new JSONObject();

        contextParam.put("projectPath", projectPath);

        contextParam.put("fileDir", file.getParent());
        String fileDirInProject = file.getParent().substring(projectPath.length());
        contextParam.put("fileDirInProject", fileDirInProject);
        contextParam.put("fileDirInProject1", "");
        contextParam.put("fileDirInProject2", "");
        contextParam.put("fileDirInProject3", "");
        if (fileDirInProject.indexOf(File.separatorChar, 1) > 0) {
            String fileDirInProject1 = fileDirInProject.substring(fileDirInProject.indexOf(File.separatorChar, 1));
            contextParam.put("fileDirInProject1", fileDirInProject1);
            if (fileDirInProject1.indexOf(File.separatorChar, 1) > 0) {
                String fileDirInProject2 = fileDirInProject1.substring(fileDirInProject1.indexOf(File.separatorChar, 1));
                contextParam.put("fileDirInProject2", fileDirInProject2);
                if (fileDirInProject2.indexOf(File.separatorChar, 1) > 0) {
                    contextParam.put("fileDirInProject3", fileDirInProject2.substring(fileDirInProject2.indexOf(File.separatorChar, 1)));
                }
            }
        }

        contextParam.put("filePath", file.getAbsolutePath());
        contextParam.put("path", file.getAbsolutePath());
        contextParam.put("fileName", file.getName());
        contextParam.put("filePrefix", file.getName().split("\\.")[0]);
        contextParam.put("fileSeparator", File.separator);
        contextParam.put("pathSeparator", File.pathSeparator);

        contextParam.put("charset", SystemUtil.get("file.encoding"));

        String fileSuffix = "";
        if (file.getName().contains(".")) {
            fileSuffix = file.getName().split("\\.")[1];
        }
        contextParam.put("fileSuffix", fileSuffix);

        contextParam.put("userHome", getUserHome());
        return contextParam;
    }

    /**
     * json的key按照字母顺序排列
     *
     * @param json
     * @return
     */
    public static JSONObject sortJsonField(JSONObject json) {
        return sortJsonField(json, false);
    }

    /**
     * json的key按照字母顺序排列
     *
     * @param json
     * @param isStruct 是否只保留数组的第一个元素
     * @return
     */
    public static JSONObject sortJsonField(JSONObject json, boolean isStruct) {

        if (json == null || json.isEmpty()) {
            return json;
        }

        TreeMap<String, Object> map = new TreeMap<>();
        for (Map.Entry<String, Object> entry : json.entrySet()) {
            Object value = entry.getValue();
            Object newVale = value;
            if (value instanceof JSONObject) {
                newVale = sortJsonField((JSONObject) value, isStruct);
            } else if (value instanceof JSONArray) {
                newVale = sortJsonField((JSONArray) value, isStruct);
            } else {
                if (isStruct) {
                    if (value instanceof String) {
                        newVale = "";
                    } else if (value instanceof Integer || value instanceof BigInteger) {
                        newVale = 0;
                    } else if (value instanceof Number) {
                        newVale = 0.0;
                    } else if (value instanceof Boolean) {
                        newVale = true;
                    }
                }
            }
            map.put(entry.getKey(), newVale);
        }
        return (JSONObject) JSON.toJSON(map, JSONWriter.Feature.WriteMapNullValue);
    }

    /**
     * 遍历json数组
     *
     * @param array
     * @return
     */
    public static JSONArray sortJsonField(JSONArray array) {
        return sortJsonField(array, false);
    }

    /**
     * 遍历json数组
     *
     * @param array
     * @param keepOnlyOneItemInArray 是否只保留数组的第一个元素
     * @return
     */
    public static JSONArray sortJsonField(JSONArray array, boolean keepOnlyOneItemInArray) {

        if (array == null || array.isEmpty()) {
            return array;
        }

        JSONArray newArray = new JSONArray(array.size());
        for (Object item : array) {
            Object newItem = item;
            if (item instanceof JSONObject) {
                newItem = sortJsonField((JSONObject) item, keepOnlyOneItemInArray);
            } else if (item instanceof JSONArray) {
                newItem = sortJsonField((JSONArray) item, keepOnlyOneItemInArray);
            }
            newArray.add(newItem);
            if (keepOnlyOneItemInArray) {
                break;
            }
        }

        return newArray;
    }

    /**
     * 获取json结构：排序，并且对于数组，只保留一个元素
     *
     * @param json
     * @return
     */
    public static JSONObject getJsonStruct(JSONObject json) {
        return sortJsonField(json, true);
    }


    /**
     * 从多个字段中获取值
     *
     * @param nodeJsonDefine
     * @param fields
     * @return
     */
    public static String getWildlyField(JSONObject nodeJsonDefine, String[] fields) {

        String path = "field";
        for (String field : fields) {
            path = nodeJsonDefine.getString(field);
            if (StrUtil.isNotBlank(path)) {
                break;
            }
        }

        return path;
    }

    /**
     * 解析json的schema
     *
     * @param jsonObject
     * @return
     */
    public static JSONObject parseJsonSchema(JSONObject jsonObject) {
        return JSONSchema.ofValue(jsonObject).toJSONObject();
    }


    /**
     * json到map转换
     *
     * @param json 入参json
     * @return 返回map
     */
    public static JSONObject convertToJsonPath(JSONObject json) {

        JSONObject newMap = new JSONObject();
        if (json == null) {
            return newMap;
        }
        for (Map.Entry<String, Object> field : json.entrySet()) {
            Object value = field.getValue();
            String fieldName = buildJsonPathKey((field.getKey()));
            if (value instanceof JSONObject || value instanceof JSONArray) {
                JSONArray array;
                boolean isArray = true;
                if (value instanceof JSONObject) {
                    array = new JSONArray(1);
                    array.add(value);
                    isArray = false;
                } else {
                    array = (JSONArray) value;
                }
                int index = 0;
                for (Object item : array) {
                    String arrayFieldName = fieldName.concat("[".concat(index + "").concat("]"));
                    if (item instanceof JSONObject) {
                        JSONObject subMap = convertToJsonPath((JSONObject) item);
                        for (Map.Entry<String, Object> entry : subMap.entrySet()) {
                            String newKey = entry.getKey();
                            if (!newKey.startsWith("[")) {
                                newKey = ".".concat(newKey);
                            }
                            String newFieldName = isArray ? arrayFieldName.concat(newKey) : fieldName.concat(newKey);
                            newMap.put((newFieldName), entry.getValue());
                        }
                    } else {
                        newMap.put((arrayFieldName), item);
                    }
                    index++;
                }
            } else {
                newMap.put((fieldName), value);
            }
        }
        return newMap;
    }

    /**
     * 处理特殊key，key中有点号，然后冲突
     *
     * @param key
     * @return
     */
    static String buildJsonPathKey(String key) {
        //TODO 避免xpath解析报错
        if (key.contains("[")) {
            key = key.replace("[", "").replace("]", "");
        }
        if (key.contains(".") || key.contains("-")) {
            return "['".concat(key).concat("']");
        }
        return key;
    }

}
