package fit.lang.plugin.json.web;

import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.HttpUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toStringMap;

/**
 * 执行节点
 */
public class ProxyJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String url = parseStringField("url", input);

        String requestPath = (String) input.getNodeContext().getAttribute(ServerJsonExecuteNode.REQUEST_PATH);

        String realUrl = url;
        if (requestPath != null) {
            if (url.endsWith("/")) {
                String servicePath = (String) input.getNodeContext().getAttribute(ServerJsonExecuteNode.SERVICE_PATH);
                String proxyPath = requestPath.substring(servicePath.length());
                if (!servicePath.endsWith("/") && !proxyPath.startsWith("/")) {
                    proxyPath = "/".concat(proxyPath);
                }
                if (proxyPath.startsWith("/")) {
                    proxyPath = proxyPath.substring(1);
                }
                realUrl = url.concat(proxyPath);
            } else {
                realUrl = url.concat(requestPath);
            }
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
        if (isJsonObjectText(responseText)) {
            result = JSONObject.parseObject(responseText);
        } else {
            result.put("_raw", responseText);
        }

        output.setData(result);

    }
}
