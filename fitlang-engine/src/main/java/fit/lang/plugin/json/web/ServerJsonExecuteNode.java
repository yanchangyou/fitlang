package fit.lang.plugin.json.web;

import cn.hutool.core.map.multi.ListValueMap;
import cn.hutool.core.util.NumberUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.crypto.SecureUtil;
import cn.hutool.http.ContentType;
import cn.hutool.http.HttpUtil;
import cn.hutool.http.server.HttpServerRequest;
import cn.hutool.http.server.HttpServerResponse;
import cn.hutool.http.server.SimpleServer;
import cn.hutool.http.server.action.Action;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.cloud.CloudServerJsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.web.server.FitServerInstance;

import java.io.File;
import java.util.*;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.*;

/**
 * 执行节点
 */
public class ServerJsonExecuteNode extends JsonExecuteNode {

    public static final String FIELD_NAME_OF_RAW = "_raw";

    /**
     * server启动文件的路径
     */
    public static String currentServerFilePath;

    /**
     * http 前缀
     */
    public static String httpPrefix = "http://127.0.0.1";

    /**
     * 默认服务器端口
     */
    public static final int DEFAULT_SERVER_PORT = 11111;

    public static final String REQUEST_PATH = "requestPath";
    public static final String SERVICE_PATH = "servicePath";

    static Map<Integer, FitServerInstance> serverMap = new HashMap<>();

    public static void setCurrentServerFilePath(String currentServerFilePath) {
        ServerJsonExecuteNode.currentServerFilePath = currentServerFilePath;
    }

    public static String getCurrentServerFilePath() {
        return currentServerFilePath;
    }

    public static String getServerFileDir() {
        if (currentServerFilePath == null) {
            return null;
        }
        int index = currentServerFilePath.lastIndexOf("/");
        if (index < 0) {
            index = currentServerFilePath.lastIndexOf("\\");
        }
        if (index < 0) {
            return null;
        }
        return convertPath(currentServerFilePath.substring(0, index));
    }

    public static void setHttpPrefix(String httpPrefix) {
        ServerJsonExecuteNode.httpPrefix = httpPrefix;
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        if (!isInPluginEnvironment()) {
            JsonDynamicFlowExecuteEngine.disableUnsafeNodes();
        }

        Integer port = buildServerPort(input.getData(), nodeJsonDefine, DEFAULT_SERVER_PORT);
        FitServerInstance fitServer = serverMap.get(port);
        JSONObject result;
        if (fitServer == null) {
            fitServer = createFitServerInstance(port);
            result = load(fitServer);
            fitServer.getSimpleServer().start();
        } else {
            result = reload(fitServer);
        }

        output.setData(result);
    }

    private void setFileServer(FitServerInstance fitServer) {
        String rootPath = nodeJsonDefine.getString("fitPath");

        if (rootPath == null) {
            rootPath = ServerJsonExecuteNode.getServerFileDir();
        }
        if (StrUtil.isBlank(rootPath) || !new File(rootPath).exists()) {
            System.out.println("server start warning: rootPath is not existed: " + rootPath);
            return;
        }
        fitServer.getSimpleServer().setRoot(rootPath);
    }

    public JSONObject load(FitServerInstance fitServer) {
        return reload(fitServer);
    }

    public JSONObject reload(FitServerInstance fitServer) {

        JSONArray serviceList = fitServer.getServiceList();
        String serverFile = fitServer.getServerFile();
        String serviceDir = fitServer.getServiceDir();

        if (serverFile == null) {
            serverFile = getCurrentServerFilePath();
        }
        fitServer.setServerFile(serverFile);

        serviceList.clear();

        JSONObject result = new JSONObject();

        setFileServer(fitServer);

        JSONArray disableInnerServiceConfig = nodeJsonDefine.getJSONArray("disableInnerService");

        if (disableInnerServiceConfig != null && disableInnerServiceConfig.contains("_api")) {
            // nothing
        } else {
            serviceList.add(addApiMenuService(fitServer));
        }

        if (disableInnerServiceConfig != null && disableInnerServiceConfig.contains("_shutdown")) {
            // nothing
        } else {
            JSONObject define = addShutdownService(fitServer);
            serviceList.add(define);
        }

        if (disableInnerServiceConfig != null && disableInnerServiceConfig.contains("_stop")) {
            // nothing
        } else {
            JSONObject stopDefine = addStopService(fitServer);
            serviceList.add(stopDefine);

            stopDefine = addStopWsService(fitServer);
            serviceList.add(stopDefine);
        }

        if (disableInnerServiceConfig != null && disableInnerServiceConfig.contains("_cloud")) {
            // nothing
        } else {
            JSONObject cloudDefine = addCloudService(fitServer);
            serviceList.add(cloudDefine);
        }

        JSONObject ipDefine = addIpService(fitServer);
        serviceList.add(ipDefine);

        if (disableInnerServiceConfig != null && disableInnerServiceConfig.contains("_reload")) {
            // nothing
        } else {
            JSONObject reloadDefine = addReloadService(fitServer);
            serviceList.add(reloadDefine);
        }

        JSONObject serviceDefine = nodeJsonDefine.getJSONObject("service");
        if (serviceDefine != null && !serviceDefine.isEmpty()) {
            loadServiceNode(nodeJsonDefine.getJSONObject("service"), fitServer);
        }

        serviceList.add(serviceDefine);

        if (StrUtil.isBlank(serviceDir)) {
            serviceDir = getServerFileDir();
        }

        if (StrUtil.isBlank(serviceDir)) {
            serviceDir = nodeJsonDefine.getString("serviceDir");
        }

        if (StrUtil.isNotBlank(serviceDir)) {
            File serverDir = new File(serviceDir);
            if (serverDir.exists()) {
                try {
                    List<JSONObject> serviceListInServerNode = loadServiceDir(serviceDir, serverDir, fitServer.getSimpleServer());
                    serviceList.addAll(serviceListInServerNode);
                } catch (Exception e) {
                    System.out.println("loadServiceDir error: " + e);
                }
            }
        }

        //添加默认的根路径
        addDefaultService(fitServer);

        fitServer.setUrl(buildUrl(fitServer.getPort(), ""));

        JSONObject defaultResult = defaultInitNode();

        result.put("message", "server start OK!");
        result.put("httpPrefix", getHttpPrefix());
        result.put("port", fitServer.getPort());
        result.put("url", buildUrl(fitServer.getPort(), ""));
        if (defaultResult != null) {
            result.put("initResult", defaultResult);
        }

        return result;
    }

    private String getHttpPrefix() {
        String httpPrefix = nodeJsonDefine.getString("httpPrefix");
        if (StrUtil.isBlank(httpPrefix)) {
            httpPrefix = ServerJsonExecuteNode.httpPrefix;
        }
        return httpPrefix;
    }

    public static Integer buildServerPort(JSONObject input, JSONObject nodeDefine, int defaultPort) {
        Integer inputPort = input.getInteger("port");
        Integer serverPort = inputPort;
        if (serverPort == null) {
            serverPort = nodeDefine.getInteger("port");
        }
        if (serverPort == null) {
            serverPort = defaultPort;
        }
        return serverPort;
    }

    private JSONObject addApiMenuService(FitServerInstance fitServerInstance) {
        return addApiMenuService(fitServerInstance, "/_api");
    }

    private JSONObject addDefaultService(FitServerInstance fitServerInstance) {
        //不存在index.html，默认使用_api
        if (!existedIndexHtml()) {
            return addApiMenuService(fitServerInstance, "/");
        }
        return null;
    }


    private JSONObject addApiMenuService(FitServerInstance fitServerInstance, String routePath) {

        clearContext(fitServerInstance.getSimpleServer(), routePath);
        fitServerInstance.getSimpleServer().addAction(routePath, new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                JSONObject welcome = getWelcomeJson(fitServerInstance);
                response.write(welcome.toJSONString(JSONWriter.Feature.PrettyFormat), getDefaultContextType());
            }
        });

        JSONObject define = new JSONObject();
        define.put("path", routePath);
        define.put("description", "show api menu");
        return define;
    }

    boolean existedIndexHtml() {
        return new File(getServerFileDir() + "/index.html").exists();
    }

    private JSONObject getWelcomeJson(FitServerInstance fitServerInstance) {
        String welcomeMessage = "hello, fit server!";
        if (nodeJsonDefine.getString("welcome") != null) {
            welcomeMessage = nodeJsonDefine.getString("welcome");
        }
        JSONObject welcome = new JSONObject();
        welcome.put("message", welcomeMessage);
        welcome.put("server", getServerDisplay(serverMap));
        welcome.put("service", getServicesDisplay(fitServerInstance));
        return welcome;
    }

    JSONArray getServerDisplay(Map<Integer, FitServerInstance> serverMap) {
        JSONArray displayList = new JSONArray();
        for (Map.Entry<Integer, FitServerInstance> serverEntry : serverMap.entrySet()) {
            FitServerInstance instance = serverEntry.getValue();
            displayList.add(instance.getDisplayInfo());
        }
        return displayList;
    }

    JSONArray getServicesDisplay(FitServerInstance fitServerInstance) {
        JSONArray display = new JSONArray();
        for (Object define : fitServerInstance.getServiceList()) {
            if (define == null) {
                continue;
            }
            JSONObject defineJson = (JSONObject) define;
            JSONObject serviceDisplay = new JSONObject();
            String servicePath = defineJson.getString("path");
            if (servicePath == null) {
                continue;
            }
            serviceDisplay.put("path", servicePath);
            serviceDisplay.put("url", buildUrl(fitServerInstance.getPort(), servicePath));
            serviceDisplay.put("loadType", defineJson.getString("loadType"));
            serviceDisplay.put("description", defineJson.get("description"));
            display.add(serviceDisplay);
        }
        return display;
    }

    private void loadServiceNode(JSONObject service, FitServerInstance fitServer) {
        if (service != null) {
            for (Map.Entry<String, Object> entry : service.entrySet()) {
                String servicePath = entry.getKey();
                JSONObject serviceDefine = (JSONObject) entry.getValue();
                if (serviceDefine == null || serviceDefine.isEmpty()) {
                    continue;
                }
                serviceDefine.put("path", servicePath);
                serviceDefine.put("loadType", "serverNode");
                registerService(fitServer.getSimpleServer(), servicePath, serviceDefine);
                fitServer.getServiceList().add(serviceDefine);
            }
        }
    }

    static String convertPath(String path) {
        return path.replace("\\", "/");
    }

    private String buildUrl(Integer serverPort, String servicePath) {
        return getHttpPrefix() + ":" + serverPort + servicePath;
    }

    static JSONObject addStopWsService(FitServerInstance fitServer) {
        String path = "/_stopWs";
        clearContext(fitServer.getSimpleServer(), path);
        fitServer.getSimpleServer().addAction(path, new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                String clientIp = getHttpClientIp(request);
                if (!isLocalIp(clientIp)) {
                    responseWriteText(request, response, "{\"message\":\"only allow stop server at host 127.0.0.1, but found: ".concat(clientIp).concat("\"}"), getDefaultContextType());
                    return;
                }
                CloudServerJsonExecuteNode.stopAll();
                responseWriteText(request, response, "{\"message\":\"stop all websocket OK!\"}", getDefaultContextType());
            }
        });
        JSONObject stopDefine = new JSONObject();
        stopDefine.put("path", path);
        stopDefine.put("description", "stop this websocket server");
        return stopDefine;
    }

    private static String getDefaultContextType() {
        return ContentType.JSON.getValue();
    }

    static JSONObject addShutdownService(FitServerInstance fitServer) {
        String path = "/_shutdown";
        clearContext(fitServer.getSimpleServer(), path);
        fitServer.getSimpleServer().addAction(path, new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                String clientIp = getHttpClientIp(request);
                if (!isLocalIp(clientIp)) {
                    responseWriteText(request, response, "{\"message\":\"only allow stop server at host 127.0.0.1, but found: ".concat(clientIp).concat("\"}"), getDefaultContextType());
                    return;
                }
                responseWriteText(request, response, "{\"message\":\"server shutdown!\"}", getDefaultContextType());

                //关闭websocket
                CloudServerJsonExecuteNode.stopAll();

                //关闭simple server
                int stopPort = fitServer.getSimpleServer().getAddress().getPort();
                serverMap.get(stopPort).getSimpleServer().getRawServer().stop(0);

                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
                System.exit(0);
            }
        });
        JSONObject stopDefine = new JSONObject();
        stopDefine.put("path", path);
        stopDefine.put("description", "stop this websocket server");
        return stopDefine;
    }

    static JSONObject addStopService(FitServerInstance fitServer) {
        String path = "/_stop";
        clearContext(fitServer.getSimpleServer(), path);
        fitServer.getSimpleServer().addAction(path, new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                String clientIp = getHttpClientIp(request);
                if (!isLocalIp(clientIp)) {
                    responseWriteText(request, response, "{\"message\":\"only allow stop server at host 127.0.0.1, but found: ".concat(clientIp).concat("\"}"), getDefaultContextType());
                    return;
                }
                int stopPort = fitServer.getSimpleServer().getAddress().getPort();

                String port = request.getParam("port");
                if (port != null) {
                    if (NumberUtil.isInteger(port)) {
                        stopPort = Integer.parseInt(port);
                    } else {
                        responseWriteText(request, response, "{\"message\":\"port must be a int number, but found: " + port + "!\"}", getDefaultContextType());
                        return;
                    }
                }

                FitServerInstance server = getFitServerInstance(stopPort);
                if (server == null) {
                    responseWriteText(request, response, "{\"message\":\"count found server at port: " + port + "!\"}", getDefaultContextType());
                    return;
                }
                responseWriteText(request, response, "{\"message\":\"stop " + stopPort + " OK!\"}", getDefaultContextType());
                server.getSimpleServer().getRawServer().stop(1);
                serverMap.remove(stopPort);
                fitServer.setRunning(false);
            }
        });
        JSONObject stopDefine = new JSONObject();
        stopDefine.put("path", path);
        stopDefine.put("description", "stop this server");
        return stopDefine;
    }

    /**
     * 启动默认执行流程
     *
     * @return
     */
    JSONObject defaultInitNode() {
        JSONObject define = nodeJsonDefine.getJSONObject("init");
        if (define == null) {
            return null;
        }

        String output = ExecuteJsonNodeUtil.executeCode(define);
        System.out.println("default init node result: " + output);
        return JSONObject.parse(output);
    }

    static JSONObject addCloudService(FitServerInstance fitServer) {
        String path = "/_cloud";
        clearContext(fitServer.getSimpleServer(), path);
        fitServer.getSimpleServer().addAction(path, new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {

                String clientId = request.getParam("clientId");

                JSONObject info = new JSONObject();
                if (StrUtil.isNotBlank(clientId)) {
                    JSONObject session = CloudServerJsonExecuteNode.getSession(clientId);
                    info.put("session", session);
                    info.put("clientId", clientId);
                    if (session == null) {
                        info.put("message", "clientId not existed!");
                    }
                } else {
                    Collection sessionList = CloudServerJsonExecuteNode.getSessions();
                    info.put("onlineCount", sessionList.size());
                    info.put("sessions", sessionList);
                }
                responseWriteText(request, response, info.toJSONString(), getDefaultContextType());
            }
        });
        JSONObject stopDefine = new JSONObject();
        stopDefine.put("path", path);
        stopDefine.put("description", "connect this server by websocket");
        return stopDefine;
    }

    static JSONObject addIpService(FitServerInstance fitServer) {
        String path = "/_ip";
        clearContext(fitServer.getSimpleServer(), path);
        fitServer.getSimpleServer().addAction(path, new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                String clientIp = getHttpClientIp(request);

                JSONObject info = new JSONObject();
                info.put("ip", clientIp);
                info.put("md5", SecureUtil.md5(clientIp));

                responseWriteText(request, response, info.toJSONString(), getDefaultContextType());
            }
        });
        JSONObject define = new JSONObject();
        define.put("path", path);
        define.put("description", "get client ip");
        return define;
    }

    private static void clearContext(SimpleServer simpleServer, String stopPath) {
        try {
            simpleServer.getRawServer().removeContext(stopPath);
        } catch (Exception e) {
            //todo ignore
        }
    }

    private JSONObject addReloadService(FitServerInstance fitServer) {
        String path = "/_reload";
        clearContext(fitServer.getSimpleServer(), path);
        fitServer.getSimpleServer().addAction(path, new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                reload(fitServer);
                JSONObject welcome = getWelcomeJson(fitServer);
                response.write(welcome.toJSONString(JSONWriter.Feature.PrettyFormat), getDefaultContextType());
            }
        });
        JSONObject reloadDefine = new JSONObject();
        reloadDefine.put("path", path);
        reloadDefine.put("description", "reload this server");
        return reloadDefine;
    }

    /**
     * 注册service
     *
     * @param simpleServer
     * @param servicePath
     * @param serviceDefine
     */
    private void registerService(SimpleServer simpleServer, String servicePath, JSONObject serviceDefine) {
        if (StrUtil.isBlank(servicePath) || serviceDefine == null || serviceDefine.isEmpty()) {
            return;
        }
        clearContext(simpleServer, servicePath);
        simpleServer.addAction(servicePath, new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                String clientIp = getHttpClientIp(request);

                JSONObject serviceDefineCopy = serviceDefine.clone();
                JSONObject contextParam = new JSONObject();
                contextParam.put(REQUEST_PATH, request.getPath());
                contextParam.put(SERVICE_PATH, servicePath);
                try {
                    JSONObject input = buildInput(request, serviceDefineCopy);
                    serviceDefineCopy.put("input", input);
                    JsonExecuteContext jsonExecuteContext = new JsonExecuteContext();
                    jsonExecuteContext.setAttribute("clientIp", clientIp);

                    String output = ExecuteJsonNodeUtil.executeCode(serviceDefineCopy, contextParam, jsonExecuteContext);
                    if (isWebNode(serviceDefineCopy)) {
                        JSONObject header = serviceDefineCopy.getJSONObject("header");
                        String contextType = null;
                        if (header != null) {
                            contextType = header.getString("contextType");
                        }
                        if (StrUtil.isNotBlank(contextType)) {
                            responseWriteText(request, response, output, contextType);
                        } else {
                            responseWriteText(request, response, output, getDefaultContextType());
                        }
                    } else {//默认json类型
                        responseWriteText(request, response, output, getDefaultContextType());
                    }
                } catch (Exception e) {
                    JSONObject result = new JSONObject();
                    result.put("message", "inner error: ".concat(e.getMessage()));
                    response.write(result.toJSONString(), getDefaultContextType());
                }
            }
        });
    }

    /**
     * 支持传递参数： jsonFormat 标识需要格式化返回
     *
     * @param request
     * @param response
     * @param output
     * @param contextType
     */
    private static void responseWriteText(HttpServerRequest request, HttpServerResponse response, String output, String contextType) {
        if (StrUtil.isBlank(contextType)) {
            contextType = getDefaultContextType();
        }
        String _jsonFormat = request.getParam("_jsonFormat");
        String text = output;
        if (isJsonObjectText(text) && "true".equals(_jsonFormat)) {
            text = JSON.parseObject(text).toJSONString(JSONWriter.Feature.PrettyFormat);
            response.write(text, ContentType.JSON.getValue());
        } else {
            response.write(text, contextType);
        }
    }

    static JSONObject buildInput(HttpServerRequest request, JSONObject serviceDefine) {

        JSONObject inputJson;
        if (serviceDefine.containsKey("input")) {
            inputJson = serviceDefine.getJSONObject("input");
        } else {
            inputJson = new JSONObject();
        }

        String requestBody = request.getBody();
        if (isJsonObjectText(requestBody)) {
            inputJson.putAll(JSONObject.parseObject(requestBody));
        } else if (isJsonArrayText(requestBody)) {
            inputJson.put("list", JSON.parseArray(requestBody));
        } else {
            if (StrUtil.isNotBlank(requestBody)) {
                inputJson.put(FIELD_NAME_OF_RAW, requestBody);
            }
        }

        ListValueMap<String, String> listValueMap = request.getParams();
        for (Map.Entry<String, List<String>> entry : listValueMap.entrySet()) {
            if (entry.getKey().startsWith("{")) {
                continue;
            }
            if (entry.getValue().isEmpty()) {
                continue;
            }
            inputJson.put(entry.getKey(), entry.getValue().get(0));
        }
        return inputJson;
    }

    List<JSONObject> loadServiceDir(String serviceRootDir, File serviceFile, SimpleServer simpleServer) {

        List<JSONObject> serviceDefineList = new ArrayList<>();
        if (serviceFile.isDirectory()) {
            File[] subFiles = serviceFile.listFiles();
            for (File subFile : subFiles) {
                List<JSONObject> subDefineList = loadServiceDir(serviceRootDir, subFile, simpleServer);
                serviceDefineList.addAll(subDefineList);
            }
        } else if (serviceFile.getName().endsWith(".fit") || serviceFile.getName().endsWith(".fit.json")) {
            String serviceDefineText = readNodeDefineFile(serviceFile);
            JSONObject serviceDefine = JSONObject.parseObject(serviceDefineText);
            String servicePath = convertPath(serviceFile.getAbsolutePath().substring(serviceRootDir.length()));
            serviceDefine.put("path", servicePath);
            serviceDefine.put("loadType", "fileSystem");
            registerService(simpleServer, servicePath, serviceDefine);
            serviceDefineList.add(serviceDefine);
        }
        return serviceDefineList;
    }

    /**
     * 是否web节点，有特殊逻辑处理
     *
     * @param flow
     * @return
     */
    public static boolean isWebNode(JSONObject flow) {
        return "web".equals(flow.getString("uni"));
    }

    public static FitServerInstance createFitServerInstance(int port) {
        FitServerInstance fitServerInstance = new FitServerInstance();
        SimpleServer simpleServer = HttpUtil.createServer(port);
        fitServerInstance.setSimpleServer(simpleServer);
        serverMap.put(port, fitServerInstance);
        fitServerInstance.setRunning(true);
        return fitServerInstance;
    }

    public static FitServerInstance getFitServerInstance(int port) {
        return serverMap.get(port);
    }
}
