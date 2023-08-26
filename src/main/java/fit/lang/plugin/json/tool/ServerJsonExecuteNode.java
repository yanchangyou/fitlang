package fit.lang.plugin.json.tool;

import cn.hutool.http.HttpUtil;
import cn.hutool.http.server.HttpServerRequest;
import cn.hutool.http.server.HttpServerResponse;
import cn.hutool.http.server.SimpleServer;
import cn.hutool.http.server.action.Action;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.HashMap;
import java.util.Map;

/**
 * 执行节点
 */
public class ServerJsonExecuteNode extends JsonExecuteNode {

    Map<Integer, SimpleServer> serverMap = new HashMap<>();

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        Integer port = nodeJsonDefine.getInteger("port");
        if (port == null) {
            port = 11111;
        }
        SimpleServer simpleServer = HttpUtil.createServer(port);
        simpleServer.addAction("/", new Action() {
            @Override
            public void doAction(HttpServerRequest request, HttpServerResponse response) {
                response.write("hello, world!");
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
        simpleServer.start();
        serverMap.put(serverPort, simpleServer);

        JSONObject result = new JSONObject();
        result.put("message", "start server at port: " + port);
        output.setData(result);

    }
}
