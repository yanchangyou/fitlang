package fit.lang.plugin.json.tool;

import cn.hutool.http.HttpUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class HttpJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String url = nodeJsonDefine.getString("url");

        String responseText = HttpUtil.post(url, input.getData());

        JSONObject data = JSONObject.parseObject(responseText);

        output.setData(data);

    }
}
