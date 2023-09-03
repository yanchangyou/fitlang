package fit.lang.plugin.json.web;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.web.websocket.WebSocketClient;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.parseNodeUrl;

/**
 * 执行节点
 * client : <a href="https://zhuanlan.zhihu.com/p/418968903">client</a>
 */
public class WebSocketClientJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String url = parseNodeUrl(input.getInputParamAndContextParam(), nodeJsonDefine);
        try {
            WebSocketClient webSocketClient = new WebSocketClient(url);
        } catch (Exception e) {
            //TODO
            throw new RuntimeException(e);
        }

        JSONObject result = new JSONObject();
        result.put("message", "ok");

        output.setData(result);
    }

}
