package fit.lang.plugin.json.tool;

import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.HttpUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonText;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toStringMap;
import static fit.lang.plugin.json.ExpressUtil.eval;

/**
 * 执行节点
 */
public class HttpJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String url = nodeJsonDefine.getString("url");

        url = (String)eval(url, input.getInputParamAndContextParam());

        HttpRequest request = HttpUtil.createPost(url);
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
        if (isJsonText(responseText)) {
            result = JSONObject.parseObject(responseText);
        } else {
            result.put("_raw", responseText);
        }

        output.setData(result);
    }
}
