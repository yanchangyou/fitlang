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
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 执行节点
 */
public class ServerJsonExecuteNode extends JsonExecuteNode {

    public static String serverRoot;

    /**
     * 默认服务器端口
     */
    public static final int DEFAULT_SERVER_PORT = 11111;
    public static final String REQUEST_PATH = "requestPath";
    public static final String ACTION_PATH = "actionPath";

    public static final String ACTION_DIR = "action";

    static Map<Integer, SimpleServer> serverMap = new HashMap<>();

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject result = new JSONObject();

        Integer port = input.getInteger("port");
        if (port == null) {
            port = nodeJsonDefine.getInteger("port");
        }
        if (port == null) {
            port = DEFAULT_SERVER_PORT;
        }

        String welcomeMessage = "hello, fit server!";

        if (nodeJsonDefine.getString("welcome") != null) {
            welcomeMessage = nodeJsonDefine.getString("welcome");
        }

        SimpleServer simpleServer = HttpUtil.createServer(port);

        Integer serverPort = port;

        JSONObject actionMap = new JSONObject();

        simpleServer.addAction("/_stop", new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                int stopPort = serverPort;
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

        JSONObject actionConfig = nodeJsonDefine.getJSONObject("action");
        if (actionConfig != null) {
            for (Map.Entry<String, Object> entry : actionConfig.entrySet()) {
                String actionPath = entry.getKey();
                JSONObject actionFlow = (JSONObject) entry.getValue();
                if (actionFlow == null || actionFlow.isEmpty()) {
                    result.put("message", "action flow is required!");
                    output.setData(result);
                    return;
                }
                registerAction(simpleServer, actionPath, actionFlow);
                actionMap.put(actionPath, "http://127.0.0.1:" + serverPort + actionPath);
            }
        }

        actionMap.put("/_stop", "http://127.0.0.1:" + serverPort + "/_stop");

        String actionDir = nodeJsonDefine.getOrDefault("actionDir", ACTION_DIR).toString();

        List<String> actionPaths = loadActionDir(actionDir, new File(actionDir), simpleServer);
        for (String path : actionPaths) {
            actionMap.put(path, "http://127.0.0.1:" + serverPort + path);
        }

        String finalWelcomeMessage = welcomeMessage;
        simpleServer.addAction("/", new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                JSONObject welcome = new JSONObject();
                welcome.put("message", finalWelcomeMessage);
                welcome.put("action", actionMap);
                response.write(welcome.toJSONString(), ContentType.JSON.getValue());
            }
        });
        simpleServer.start();
        serverMap.put(serverPort, simpleServer);
        result.put("message", "start server at port: " + port);

        output.setData(result);
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
        simpleServer.addAction(actionPath, new Action() {
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

    List<String> loadActionDir(String actionRootDir, File actionFile, SimpleServer simpleServer) {

        List<String> actionPaths = new ArrayList<>();
        if (actionFile.isDirectory()) {
            File[] subFiles = actionFile.listFiles();
            for (File subFile : subFiles) {
                List<String> subPaths = loadActionDir(actionRootDir, subFile, simpleServer);
                actionPaths.addAll(subPaths);
            }
        } else {
            String actionDefine = FileUtil.readUtf8String(actionFile);
            JSONObject actionFlow = JSONObject.parseObject(actionDefine);
            String actionPath = actionFile.getAbsolutePath().replace(actionRootDir, "");
            registerAction(simpleServer, actionPath, actionFlow);
            actionPaths.add(actionPath);
        }
        return actionPaths;
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
