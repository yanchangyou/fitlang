package fit.lang.plugin.json.tool;

import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.HttpUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.SocketAddress;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.*;

/**
 * 执行节点
 */
public class HttpJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String url = parseNodeUrl(input.getInputParamAndContextParam(), nodeJsonDefine);

        HttpRequest request = HttpUtil.createPost(url);
        JSONObject header = nodeJsonDefine.getJSONObject("header");
        if (header != null && !header.isEmpty()) {
            request.addHeaders(toStringMap(header));
        }
        String httpBody;
        Object httpParam = nodeJsonDefine.get("param");
        if (httpParam != null) {
            Object param = ExpressUtil.eval(httpParam, input.getInputParamAndContextParam());
            if (param != null) {
                httpBody = param.toString();
            } else {
                httpBody = "";
            }
        } else {
            httpBody = input.getData().toJSONString();
        }

        JSONObject proxy = nodeJsonDefine.getJSONObject("proxy");
        if (proxy != null) {
            String type = proxy.getString("type");
            SocketAddress socketAddress = new InetSocketAddress(proxy.getString("host"), proxy.getInteger("port"));
            Proxy.Type typeEnum;
            if ("http".equals(type) || "HTTP".equals(type)) {
                typeEnum = Proxy.Type.HTTP;
            } else if ("socket".equals(type) || "socks".equals(type) || "SOCKS".equals(type)) {
                typeEnum = Proxy.Type.SOCKS;
            } else {
                typeEnum = Proxy.Type.DIRECT;
            }
            request.setProxy(new Proxy(typeEnum, socketAddress));
        }
        request.body(httpBody);
        HttpResponse response = request.execute();
        String responseText = response.body();

        if (responseText == null) {
            responseText = "";
        }
        JSONObject result = new JSONObject(1);
        if (isJsonText(responseText)) {
            result = JSONObject.parseObject(responseText);
        } else {
            result.put("_raw", responseText);
        }

        output.setData(result);
    }
}
