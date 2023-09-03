package fit.lang.plugin.json.tool;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.tool.server.FitServerInstance;

import static fit.lang.plugin.json.tool.ServerJsonExecuteNode.addStopService;

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

        if (rootPath == null) {
            rootPath = ServerJsonExecuteNode.getServerFileDir();
        }

        FitServerInstance fitServerInstance = ServerJsonExecuteNode.createFitServerInstance(port);
        fitServerInstance.getSimpleServer().setRoot(rootPath);
        fitServerInstance.getSimpleServer().start();

        fitServerInstance.setServerFile(ServerJsonExecuteNode.getCurrentServerFilePath());
        fitServerInstance.setUrl("http://127.0.0.1:" + port);

        ServerJsonExecuteNode.serverMap.put(port, fitServerInstance);

        addStopService(fitServerInstance);

        result.put("message", "start server at port: " + port);

        output.setData(result);
    }
}
