package fit.lang.plugin.json.http;

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
public class HttpPostJsonJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        request(input, output, nodeJsonDefine, Method.POST);
    }

    public static void request(JsonExecuteNodeInput input, JsonExecuteNodeOutput output, JSONObject nodeJsonDefine, Method method) {

        String url = ExecuteJsonNodeUtil.parseStringField("url", input, nodeJsonDefine);

        HttpRequest request = HttpUtil.createRequest(method, url);

        setHttpHeader(nodeJsonDefine.getJSONObject("header"), request);

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

        setProxy(nodeJsonDefine.getJSONObject("proxy"), request);

        request.body(httpBody);
        HttpResponse response = request.execute();

        JSONObject result = parseHttpResult(response);

        output.setData(result);

    }

}
