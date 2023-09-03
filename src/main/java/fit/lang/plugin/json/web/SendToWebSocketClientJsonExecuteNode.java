package fit.lang.plugin.json.web;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.web.websocket.WebSocketClient;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.parseNodeUrl;

/**
 * 执行节点:发送到启动了，webSocketClient, 模拟请求，默认时后台运行无法调用
 */
public class SendToWebSocketClientJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        WebSocketClient webSocketClient = WebSocketClientJsonExecuteNode.getWebSocketClient();

        webSocketClient.send(input.getData().toJSONString());

        JSONObject result = new JSONObject();
        result.put("message", "ok");

        output.setData(result);
    }

}
