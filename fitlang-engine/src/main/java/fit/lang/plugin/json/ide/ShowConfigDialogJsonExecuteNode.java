package fit.lang.plugin.json.ide;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ShowConfigDialogJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject config = nodeJsonDefine.getJSONObject("config");
        JSONObject option = nodeJsonDefine.getJSONObject("option");

        JSONObject newConfig = UserIdeManager.getUserIdeInterface().showNodeConfigDialog(config, option);

        String configField = nodeJsonDefine.getString("configField");

        if (StrUtil.isBlank(configField)) {
            configField = "config";
        }

        input.getNodeContext().setAttribute(configField, newConfig);

        output.set(configField, newConfig);
    }
}
