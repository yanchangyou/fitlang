package fit.lang.plugin.json.http;

import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.HttpUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.*;

/**
 * 执行节点
 */
public class HttpPostFormJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String url = parseNodeUrl(input.getInputParamAndContextParam(), nodeJsonDefine);

        HttpRequest request = HttpUtil.createPost(url);

        setHttpHeader(nodeJsonDefine.getJSONObject("header"), request);

        Object httpParam = nodeJsonDefine.get("param");

        parseHttpFormParam(input, request, httpParam);

        setProxy(nodeJsonDefine.getJSONObject("proxy"), request);

        HttpResponse response = request.execute();

        JSONObject result = parseHttpResult(response);

        output.setData(result);
    }

}
