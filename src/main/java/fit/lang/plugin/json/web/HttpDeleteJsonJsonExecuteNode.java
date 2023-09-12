package fit.lang.plugin.json.web;

import cn.hutool.http.Method;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.web.HttpPostJsonJsonExecuteNode.request;

/**
 * 执行节点
 */
public class HttpDeleteJsonJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        request(input, output, nodeJsonDefine, Method.DELETE);
    }

}
