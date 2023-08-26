package fit.lang.plugin.json.tool;

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

import java.util.HashMap;
import java.util.Map;

/**
 * 执行节点
 */
public class ServerJsonExecuteNode extends JsonExecuteNode {

    /**
     * 默认服务器端口
     */
    public static final int DEFAULT_SERVER_PORT = 11111;

    Map<Integer, SimpleServer> serverMap = new HashMap<>();

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject result = new JSONObject();

        Integer port = nodeJsonDefine.getInteger("port");
        if (port == null) {
            port = DEFAULT_SERVER_PORT;
        }

        SimpleServer simpleServer = HttpUtil.createServer(port);
        simpleServer.addAction("/", new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                response.write("{\"message\":\"hello, fit server!\"}", ContentType.JSON.getValue());
            }
        });
        Integer serverPort = port;
        simpleServer.addAction("/stop", new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                response.write("{\"message\":\"stop OK!\"}");
                serverMap.get(serverPort).getRawServer().stop(1);
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
                simpleServer.addAction(actionPath, new Action() {
                    @Override
                    public void doAction(HttpServerRequest request, HttpServerResponse response) {
                        String input = request.getBody();
                        String output = ExecuteJsonNodeUtil.executeCode(input, actionFlow.toJSONString());
                        response.write(output, ContentType.JSON.getValue());
                    }
                });
            }
        }

        simpleServer.start();
        serverMap.put(serverPort, simpleServer);
        result.put("message", "start server at port: " + port);

        output.setData(result);
    }
}
