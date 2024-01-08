package fit.lang.plugin.json.ide;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ReadConfigJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String configField = nodeJsonDefine.getString("configField");

        if (StrUtil.isBlank(configField)) {
            configField = "config";
        }

        JSONObject config = UserIdeManager.getUserIdeInterface().getNodeConfig();

        input.getNodeContext().setAttribute(configField, config);

        output.set(configField, config);
    }
}
