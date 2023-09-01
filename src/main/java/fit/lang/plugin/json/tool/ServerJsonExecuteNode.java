package fit.lang.plugin.json.tool;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.map.multi.ListValueMap;
import cn.hutool.core.util.NumberUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.http.ContentType;
import cn.hutool.http.HttpUtil;
import cn.hutool.http.server.HttpServerRequest;
import cn.hutool.http.server.HttpServerResponse;
import cn.hutool.http.server.SimpleServer;
import cn.hutool.http.server.action.Action;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 执行节点
 */
public class ServerJsonExecuteNode extends JsonExecuteNode {

    /**
     * server启动文件的路径
     */
    public static String serverFilePath;

    /**
     * 默认服务器端口
     */
    public static final int DEFAULT_SERVER_PORT = 11111;
    public static final String REQUEST_PATH = "requestPath";
    public static final String SERVICE_PATH = "servicePath";

    static Map<Integer, SimpleServer> serverMap = new HashMap<>();

    static Map<Integer, JSONObject> serverMetaMap = new HashMap<>();

    public static void setServerFilePath(String serverFilePath) {
        ServerJsonExecuteNode.serverFilePath = serverFilePath;
    }

    public static String getServerFilePath() {
        return serverFilePath;
    }

    public static String getServerFileDir() {
        if (serverFilePath == null) {
            return null;
        }
        int index = serverFilePath.lastIndexOf("/");
        if (index < 0) {
            index = serverFilePath.lastIndexOf("\\");
        }
        if (index < 0) {
            return null;
        }
        return convertPath(serverFilePath.substring(0, index));
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject result = new JSONObject();

        JSONObject meta = new JSONObject();

        SimpleServer simpleServer = HttpUtil.createServer(buildServerPort(input));

        JSONArray serviceList = new JSONArray();

        JSONObject stopDefine = addStopService(simpleServer);
        serviceList.add(stopDefine);

        JSONObject serviceConfig = nodeJsonDefine.getJSONObject("service");
        if (serviceConfig != null && !serviceConfig.isEmpty()) {
            loadServiceNode(simpleServer, serviceConfig, serviceList);
        }

        serviceList.add(serviceConfig);

        String serviceDir = getServerFileDir();
        meta.put("file", getServerFilePath());

        if (serviceDir == null) {
            serviceDir = nodeJsonDefine.getString("serviceDir");
        }

        if (serviceDir != null) {
            try {
                List<JSONObject> serviceListInServerNode = loadServiceDir(serviceDir, new File(serviceDir), simpleServer);
                serviceList.addAll(serviceListInServerNode);
            } catch (Exception e) {
                System.out.println("loadServiceDir error: " + e);
            }
        }
        addRootService(nodeJsonDefine, simpleServer, serviceList);

        simpleServer.start();

        serverMap.put(simpleServer.getAddress().getPort(), simpleServer);
        meta.put("url", getHttpPrefix().concat(":".concat(String.valueOf(simpleServer.getAddress().getPort()))));
        serverMetaMap.put(simpleServer.getAddress().getPort(), meta);

        result.put("message", "start server at port: " + simpleServer.getAddress().getPort());

        output.setData(result);
    }

    private String getHttpPrefix() {
        String httpPrefix = nodeJsonDefine.getString("httpPrefix");
        if (StrUtil.isBlank(httpPrefix)) {
            httpPrefix = "http://127.0.0.1";
        }
        return httpPrefix;
    }

    @NotNull
    private Integer buildServerPort(JsonExecuteNodeInput input) {
        Integer serverPort = input.getInteger("port");
        if (serverPort == null) {
            serverPort = nodeJsonDefine.getInteger("port");
        }
        if (serverPort == null) {
            serverPort = DEFAULT_SERVER_PORT;
        }
        return serverPort;
    }

    private void addRootService(JSONObject serverConfig, SimpleServer simpleServer, JSONArray serviceDefines) {

        String welcomeMessage = "hello, fit server!";

        if (serverConfig.getString("welcome") != null) {
            welcomeMessage = serverConfig.getString("welcome");
        }

        String finalWelcomeMessage = welcomeMessage;
        simpleServer.addAction("/", new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                JSONObject welcome = new JSONObject();
                welcome.put("message", finalWelcomeMessage);
                welcome.put("server", serverMetaMap.values());
                welcome.put("service", getServicesDisplay(serviceDefines, simpleServer.getAddress().getPort()));
                response.write(welcome.toJSONString(JSONWriter.Feature.PrettyFormat), ContentType.JSON.getValue());
            }
        });
    }

    JSONArray getServicesDisplay(JSONArray serviceDefines, Integer serverPort) {
        JSONArray display = new JSONArray();
        for (Object define : serviceDefines) {
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
            serviceDisplay.put("url", buildUrl(serverPort, servicePath));
            serviceDisplay.put("loadType", defineJson.getString("loadType"));
            serviceDisplay.put("description", defineJson.get("description"));
            display.add(serviceDisplay);
        }
        return display;
    }

    private void loadServiceNode(SimpleServer simpleServer, JSONObject serviceConfig, JSONArray serviceDefines) {
        if (serviceConfig != null) {
            for (Map.Entry<String, Object> entry : serviceConfig.entrySet()) {
                String servicePath = entry.getKey();
                JSONObject serviceFlow = (JSONObject) entry.getValue();
                if (serviceFlow == null || serviceFlow.isEmpty()) {
                    continue;
                }
                JSONObject serviceDefine = buildStandardServiceDefine(serviceFlow);
                serviceDefine.put("path", servicePath);
                serviceDefine.put("loadType", "serverNode");
                registerService(simpleServer, servicePath, serviceDefine);
                serviceDefines.add(serviceDefine);
            }
        }
    }

    static String convertPath(String path) {
        return path.replace("\\", "/");
    }

    JSONObject buildStandardServiceDefine(JSONObject serviceDefine) {
        JSONObject standardServiceDefine = serviceDefine;
        if (!serviceDefine.containsKey("flow")) {
            standardServiceDefine = new JSONObject();
            standardServiceDefine.put("flow", serviceDefine);
            standardServiceDefine.put("input", new JSONObject());
        }
        return standardServiceDefine;
    }

    @NotNull
    private String buildUrl(Integer serverPort, String servicePath) {
        return getHttpPrefix() + ":" + serverPort + servicePath;
    }

    private JSONObject addStopService(SimpleServer simpleServer) {
        String stopPath = "/_stop";
        simpleServer.addAction(stopPath, new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                int stopPort = simpleServer.getAddress().getPort();
                String port = request.getParam("port");
                if (port != null) {
                    if (NumberUtil.isInteger(port)) {
                        stopPort = Integer.parseInt(port);
                    } else {
                        response.write("{\"message\":\"port must be a int number, but found: " + port + "!\"}");
                        return;
                    }
                }

                SimpleServer server = serverMap.get(stopPort);
                if (server == null) {
                    response.write("{\"message\":\"count found server at port: " + port + "!\"}");
                    return;
                }
                response.write("{\"message\":\"stop " + stopPort + " OK!\"}");
                server.getRawServer().stop(1);
                serverMap.remove(stopPort);
                serverMetaMap.remove(stopPort);
            }
        });
        JSONObject stopDefine = new JSONObject();
        stopDefine.put("path", stopPath);
        stopDefine.put("description", "stop this server");
        return stopDefine;
    }

    /**
     * 注册service
     *
     * @param simpleServer
     * @param servicePath
     * @param serviceConfig
     */
    private void registerService(SimpleServer simpleServer, String servicePath, JSONObject serviceConfig) {
        if (StrUtil.isBlank(servicePath) || serviceConfig == null || serviceConfig.isEmpty()) {
            return;
        }
        simpleServer.addAction(servicePath, new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                String requestBody = request.getBody();
                if (StrUtil.isBlank(requestBody)) {
                    requestBody = "{}";
                }
                JSONObject defaultInput = new JSONObject();
                JSONObject serviceFlow = serviceConfig;
                if (serviceConfig.containsKey("input") && serviceConfig.containsKey("flow")) {
                    defaultInput = serviceConfig.getJSONObject("input");
                    serviceFlow = serviceConfig.getJSONObject("flow");
                }

                JSONObject inputJson = defaultInput;

                if (requestBody.startsWith("{") && requestBody.endsWith("}")) {
                    inputJson.putAll(JSONObject.parseObject(requestBody));
                }

                ListValueMap<String, String> listValueMap = request.getParams();
                for (Map.Entry<String, List<String>> entry : listValueMap.entrySet()) {
                    inputJson.put(entry.getKey(), entry.getValue().get(0));
                }
                JSONObject contextParam = new JSONObject();
                contextParam.put(REQUEST_PATH, request.getPath());
                contextParam.put(SERVICE_PATH, servicePath);
                try {
                    String output = ExecuteJsonNodeUtil.executeCode(inputJson, serviceFlow, contextParam);
                    if (isWebNode(serviceFlow)) {
                        JSONObject header = serviceFlow.getJSONObject("header");
                        String contextType = null;
                        if (header != null) {
                            contextType = header.getString("contextType");
                        }
                        if (StrUtil.isNotBlank(contextType)) {
                            response.write(output, contextType);
                        } else {
                            response.write(output);
                        }
                    } else {//默认json类型
                        response.write(output, ContentType.JSON.getValue());
                    }
                } catch (Exception e) {
                    JSONObject result = new JSONObject();
                    result.put("message", "inner error: ".concat(e.getMessage()));
                    response.write(result.toJSONString(), ContentType.JSON.getValue());
                }
            }
        });
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
            String serviceDefine = FileUtil.readUtf8String(serviceFile);
            JSONObject serviceDefineJson = buildStandardServiceDefine(JSONObject.parseObject(serviceDefine));
            String servicePath = convertPath(serviceFile.getAbsolutePath().substring(serviceRootDir.length()));
            serviceDefineJson.put("path", servicePath);
            serviceDefineJson.put("loadType", "fileSystem");
            registerService(simpleServer, servicePath, serviceDefineJson);
            serviceDefineList.add(serviceDefineJson);
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
}
