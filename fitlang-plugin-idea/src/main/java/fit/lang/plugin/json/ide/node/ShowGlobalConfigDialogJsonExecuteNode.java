package fit.lang.plugin.json.ide.node;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ShowGlobalConfigDialogJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String configField = nodeJsonDefine.getString("configField");

        if (StrUtil.isBlank(configField)) {
            configField = "config";
        }

        Object config = parseField(configField, input);

        if (!(config instanceof JSONObject)) {
            config = nodeJsonDefine.getJSONObject(configField);
        }

        JSONObject option = nodeJsonDefine.getJSONObject("option");

        JSONObject newConfig = UserIdeManager.getUserIdeInterface().showGlobalConfigDialog((JSONObject) config, option);

        input.getNodeContext().setAttribute(configField, newConfig);

        output.set(configField, newConfig);
    }
}
