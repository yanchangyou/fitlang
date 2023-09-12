package fit.lang.plugin.json.web;

import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.HttpUtil;
import com.alibaba.fastjson2.JSONObject;
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

        String url = parseNodeUrl(input.getInputParamAndContextParam(), nodeJsonDefine);

        HttpRequest request = HttpUtil.createPost(url);

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
