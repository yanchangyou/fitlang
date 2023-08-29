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

    static String ACTION_PREFIX = "/action";

    /**
     * 默认服务器端口
     */
    public static final int DEFAULT_SERVER_PORT = 11111;
    public static final String REQUEST_PATH = "requestPath";
    public static final String ACTION_PATH = "actionPath";

    public static final String ACTION_DIR = "action";

    static Map<Integer, SimpleServer> serverMap = new HashMap<>();

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
        return serverFilePath.substring(0, serverFilePath.lastIndexOf(File.separatorChar));
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject result = new JSONObject();

        SimpleServer simpleServer = HttpUtil.createServer(buildServerPort(input));

        JSONArray actionList = new JSONArray();

        JSONObject stopDefine = addStopAction(simpleServer);
        actionList.add(stopDefine);

        JSONObject actionConfig = nodeJsonDefine.getJSONObject("action");
        loadActionNode(simpleServer, actionConfig, actionList);

        actionList.add(actionConfig);

        String actionDir = getServerFileDir();
        if (actionDir == null) {
            actionDir = nodeJsonDefine.getString("actionDir");
        } else {
            actionDir = actionDir + ACTION_DIR;
        }

        List<JSONObject> actionListInServerNode = loadActionDir(actionDir, new File(actionDir), simpleServer);
        actionList.addAll(actionListInServerNode);


        addRootAction(nodeJsonDefine, simpleServer, actionList);

        simpleServer.start();

        serverMap.put(simpleServer.getAddress().getPort(), simpleServer);

        result.put("message", "start server at port: " + simpleServer.getAddress().getPort());

        output.setData(result);
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

    private void addRootAction(JSONObject serverConfig, SimpleServer simpleServer, JSONArray actionDefines) {

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
                welcome.put("action", getActionsDisplay(actionDefines, simpleServer.getAddress().getPort()));
                response.write(welcome.toJSONString(), ContentType.JSON.getValue());
            }
        });
    }

    JSONArray getActionsDisplay(JSONArray actionDefines, Integer serverPort) {
        JSONArray display = new JSONArray();
        for (Object define : actionDefines) {
            if (define == null) {
                continue;
            }
            JSONObject defineJson = (JSONObject) define;
            JSONObject actionDisplay = new JSONObject();
            String actionPath = defineJson.getString("path");
            actionDisplay.put("path", actionPath);
            actionDisplay.put("url", buildUrl(serverPort, actionPath));

            actionDisplay.put("description", defineJson.get("description"));
            display.add(actionDisplay);
        }
        return display;
    }

    private void loadActionNode(SimpleServer simpleServer, JSONObject actionConfig, JSONArray actionDefines) {
        if (actionConfig != null) {
            for (Map.Entry<String, Object> entry : actionConfig.entrySet()) {
                String actionPath = entry.getKey();
                JSONObject actionFlow = (JSONObject) entry.getValue();
                if (actionFlow == null || actionFlow.isEmpty()) {
                    continue;
                }
                JSONObject actionDefine = buildStandardActionDefine(actionFlow);
                actionDefine.put("path", actionPath);
                registerAction(simpleServer, actionPath, actionDefine);
                actionDefines.add(actionDefine);
            }
        }
    }

    JSONObject buildStandardActionDefine(JSONObject actionDefine) {
        JSONObject standardActionDefine = actionDefine;
        if (!actionDefine.containsKey("flow")) {
            standardActionDefine = new JSONObject();
            standardActionDefine.put("flow", actionDefine);
            standardActionDefine.put("input", new JSONObject());
        }
        return standardActionDefine;
    }

    @NotNull
    private String buildUrl(Integer serverPort, String actionPath) {
        return "http://127.0.0.1:" + serverPort + ACTION_PREFIX + actionPath;
    }

    private JSONObject addStopAction(SimpleServer simpleServer) {
        String stopPath = "/_stop";
        simpleServer.addAction(ACTION_PREFIX + stopPath, new Action() {
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
            }
        });
        JSONObject stopDefine = new JSONObject();
        stopDefine.put("path", stopPath);
        stopDefine.put("description", "stop this server");
        return stopDefine;
    }

    /**
     * 注册action
     *
     * @param simpleServer
     * @param actionPath
     * @param actionConfig
     */
    private void registerAction(SimpleServer simpleServer, String actionPath, JSONObject actionConfig) {
        if (StrUtil.isBlank(actionPath) || actionConfig == null || actionConfig.isEmpty()) {
            return;
        }
        simpleServer.addAction(ACTION_PREFIX + actionPath, new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                String requestBody = request.getBody();
                if (StrUtil.isBlank(requestBody)) {
                    requestBody = "{}";
                }
                JSONObject defaultInput = new JSONObject();
                JSONObject actionFlow = actionConfig;
                if (actionConfig.containsKey("input") && actionConfig.containsKey("flow")) {
                    defaultInput = actionConfig.getJSONObject("input");
                    actionFlow = actionConfig.getJSONObject("flow");
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
                contextParam.put(ACTION_PATH, actionPath);
                try {
                    String output = ExecuteJsonNodeUtil.executeCode(inputJson, actionFlow, contextParam);
                    if (isWebNode(actionFlow)) {
                        JSONObject header = actionFlow.getJSONObject("header");
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

    List<JSONObject> loadActionDir(String actionRootDir, File actionFile, SimpleServer simpleServer) {

        List<JSONObject> actionDefineList = new ArrayList<>();
        if (actionFile.isDirectory()) {
            File[] subFiles = actionFile.listFiles();
            for (File subFile : subFiles) {
                List<JSONObject> subDefineList = loadActionDir(actionRootDir, subFile, simpleServer);
                actionDefineList.addAll(subDefineList);
            }
        } else {
            String actionDefine = FileUtil.readUtf8String(actionFile);
            JSONObject actionDefineJson = buildStandardActionDefine(JSONObject.parseObject(actionDefine));
            String actionPath = actionFile.getAbsolutePath().replace(actionRootDir, "");
            actionDefineJson.put("path", actionPath);
            registerAction(simpleServer, actionPath, actionDefineJson);
            actionDefineList.add(actionDefineJson);
        }
        return actionDefineList;
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
