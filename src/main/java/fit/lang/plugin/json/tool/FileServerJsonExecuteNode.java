package fit.lang.plugin.json.tool;

import cn.hutool.http.HttpUtil;
import cn.hutool.http.server.SimpleServer;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.tool.server.FitServerInstance;

/**
 * 执行节点
 */
public class FileServerJsonExecuteNode extends JsonExecuteNode {

    /**
     * 默认服务器端口
     */
    public static final int DEFAULT_SERVER_PORT = 10111;

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject result = new JSONObject();

        Integer port = nodeJsonDefine.getInteger("port");
        if (port == null) {
            port = DEFAULT_SERVER_PORT;
        }

        String rootPath = nodeJsonDefine.getString("root");

        SimpleServer simpleServer = HttpUtil.createServer(port);
        simpleServer.setRoot(rootPath);

        simpleServer.start();
        result.put("message", "start server at port: " + port);

        FitServerInstance fitServerInstance = ServerJsonExecuteNode.createFitServerInstance(port);
        fitServerInstance.setSimpleServer(simpleServer);
        ServerJsonExecuteNode.serverMap.put(port, fitServerInstance);

        output.setData(result);
    }
}
