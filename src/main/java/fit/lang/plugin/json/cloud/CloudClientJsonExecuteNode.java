package fit.lang.plugin.json.cloud;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.web.websocket.WebSocketClient;
import fit.lang.plugin.json.web.websocket.WebSocketClientHandler;

import java.util.HashMap;
import java.util.Map;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonText;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.parseNodeUrl;

/**
 *
 */
public class CloudClientJsonExecuteNode extends JsonExecuteNode {

    /**
     * 限定，一个服务器url只能连接一次
     */
    static Map<String, String> serverSessionMap = new HashMap<>();

    boolean isDebugMode() {
        return Boolean.TRUE.equals(nodeJsonDefine.getBoolean("debugMode"));
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String url = parseNodeUrl(input.getInputParamAndContextParam(), nodeJsonDefine, "cloudServer");
        String sessionId = serverSessionMap.get(url);
        //同一个url只能连接一次
        if (sessionId == null || isDebugMode()) {

            WebSocketClient webSocketClient;
            try {
                webSocketClient = new WebSocketClient(url);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }

            webSocketClient.send(input.getData().toJSONString());

            //等待10秒
            for (int i = 0; i < 1000; i++) {
                if (!WebSocketClientHandler.resultList.isEmpty()) {
                    break;
                }
                try {
                    Thread.sleep(10);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }

            String data = WebSocketClientHandler.resultList.get(0);
            WebSocketClientHandler.resultList.remove(0);

            JSONObject result = new JSONObject();
            result.put("message", "ok");
            if (isJsonText(data)) {
                JSONObject responseJson = JSONObject.parse(data);
                sessionId = responseJson.getString("sessionId");
                serverSessionMap.put(url, sessionId);
                result.put("meta", responseJson);
            } else {
                result.put("meta", data);
            }

            if (data == null) {
                result.put("message", "timeout!");
            }
            output.setData(result);
        } else {
            JSONObject result = new JSONObject();
            result.put("message", "already connected");
            JSONObject responseJson = new JSONObject();
            responseJson.put("sessionId", sessionId);
            result.put("meta", responseJson);
            output.setData(result);
        }
    }

}