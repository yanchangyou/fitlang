package fit.lang.plugin.json.http;

import cn.hutool.http.Method;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.http.HttpJsonExecuteNode.request;

/**
 * 执行节点
 */
public class HttpHeadJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        request(input, output, nodeJsonDefine, Method.HEAD);
    }
}
