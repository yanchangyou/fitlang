package fit.lang.plugin.json.applet;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.define.ExecuteNode;
import fit.lang.define.ExecuteNodeBuildable;
import fit.lang.define.ExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.Set;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public class AppletJsonExecuteNode extends JsonExecuteNode implements ExecuteNodeBuildable {

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
        inputJson = parseRealFormData(inputJson);
        if (outputJson == null) {
            outputJson = new JSONObject();
        }
        nodeDefine.put("child", scriptJson);
        ExecuteNodeUtil.buildChildNode(this, nodeDefine);
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

    /**
     * 获取真实数据，input支持$N格式，表示入参元数据定义：
     * $1 dataType
     * $2 editType
     * $3 title
     *
     * @param formData
     * @return
     */
    public static JSONObject parseRealFormData(JSONObject formData) {
        JSONObject realFormData = new JSONObject();
        for (String key : formData.keySet()) {
            String realKey = key.split("\\$")[0];
            realFormData.put(realKey, formData.get(key));
        }
        return realFormData;
    }

    public static JSONObject buildOutputData(JSONObject outputDefine, JSONObject rawOutputData) {
        JSONObject outputData = new JSONObject();
        Set<String> defineKeys = outputDefine.keySet();
        for (String key : rawOutputData.keySet()) {
            String newKey = key;
            for (String outputKey : defineKeys) {
                if (outputKey.startsWith(key.concat("$"))) {
                    newKey = outputKey;
                    break;
                }
            }
            outputData.put(newKey, rawOutputData.get(key));
        }
        Set<String> newKeys = outputData.keySet();
        //补充缺少的key， TODO data type deal
        for (String defineKey : defineKeys) {
            if (!newKeys.contains(defineKey)) {
                outputData.put(defineKey, "");
            }
        }
        return outputData;
    }

}
