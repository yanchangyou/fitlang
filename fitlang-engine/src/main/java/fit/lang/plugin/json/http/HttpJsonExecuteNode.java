package fit.lang.plugin.json.http;

import cn.hutool.core.util.StrUtil;
import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.HttpUtil;
import cn.hutool.http.Method;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.*;

/**
 * 执行节点
 */
public class HttpJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        String method = nodeJsonDefine.getString("method");
        if (StrUtil.isBlank(method)) {
            method = "POST";
        }
        request(input, output, nodeJsonDefine, Method.valueOf(method));
    }

    public static void request(JsonExecuteNodeInput input, JsonExecuteNodeOutput output, JSONObject nodeJsonDefine, Method method) {

        String url = ExecuteJsonNodeUtil.parseStringField("url", input, nodeJsonDefine);

        HttpRequest request = HttpUtil.createRequest(method, url);

        setHttpHeader(nodeJsonDefine.getJSONObject("header"), request);
        setProxy(nodeJsonDefine.getJSONObject("proxy"), request);

        JSONObject httpParam = parseParam(nodeJsonDefine);

        Boolean isPostForm = nodeJsonDefine.getBoolean("isPostForm");
        if (method == Method.GET || (method == Method.POST && Boolean.TRUE.equals(isPostForm))) {
            parseHttpFormParam(input, request, httpParam);
        } else if (method == Method.POST || method == Method.PUT || method == Method.DELETE) {
            String httpBody;
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
            request.body(httpBody);
        }

        HttpResponse response = request.execute();

        JSONObject result = parseHttpResult(response);

        output.setData(result);

    }

    private static JSONObject parseParam(JSONObject nodeJsonDefine) {
        JSONObject httpParam = new JSONObject();
        for (int i = 0; i < 10; i++) {
            String key = "param" + (i > 0 ? i + "" : "");
            Object param = nodeJsonDefine.get(key);
            if (param instanceof JSONObject) {
                httpParam.putAll((JSONObject) param);
            }
        }
        return httpParam;
    }

}
