package fit.lang.plugin.json.app;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.define.ExecuteNode;
import fit.lang.define.ExecuteNodeBuildable;
import fit.lang.define.ExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public class MiniAppJsoMinnExecuteNode extends JsonExecuteNode implements ExecuteNodeBuildable {

    JSONObject scriptJson;

    JSONObject inputJson;

    JSONObject outputJson;

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        JSONObject nodeDefine = getJsonData(executeNodeData);
        scriptJson = nodeDefine.getJSONObject("script");
        if (scriptJson == null) {
            scriptJson = new JSONObject();
            scriptJson.put("uni", "hello");
        }

        inputJson = nodeDefine.getJSONObject("input");
        outputJson = nodeDefine.getJSONObject("output");
        if (inputJson == null) {
            inputJson = new JSONObject();
        }
        if (outputJson == null) {
            outputJson = new JSONObject();
        }
        ExecuteNodeUtil.buildChildNode(this, scriptJson);
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        inputJson.putAll(input.getData());
        input.setData(inputJson);

        output.setData(outputJson);

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        for (ExecuteNode childNode : childNodes) {
            childNode.execute(input, output);
        }

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }
}
