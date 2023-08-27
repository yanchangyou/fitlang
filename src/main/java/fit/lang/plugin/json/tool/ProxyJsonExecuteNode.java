package fit.lang.plugin.json.tool;

import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.HttpUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toStringMap;

/**
 * 执行节点
 */
public class ProxyJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String url = nodeJsonDefine.getString("url");

        String requestPath = (String) input.getNodeContext().getAttribute(ServerJsonExecuteNode.REQUEST_PATH);

        String realUrl = url;
        if (requestPath != null) {
            realUrl = url.concat(requestPath);
        }

        HttpRequest request = HttpUtil.createPost(realUrl);
        JSONObject header = nodeJsonDefine.getJSONObject("header");
        if (header != null && !header.isEmpty()) {
            request.addHeaders(toStringMap(header));
        }
        request.body(input.getData().toJSONString());
        HttpResponse response = request.execute();
        String responseText = response.body();

        if (responseText == null) {
            responseText = "";
        }
        JSONObject result = new JSONObject(1);
        if (responseText.startsWith("{") && responseText.endsWith("}")) {
            result = JSONObject.parseObject(responseText);
        } else {
            result.put("_raw", responseText);
        }

        output.setData(result);

    }
}
