package fit.lang.plugin.json.ide.node;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeConst.FIELD_NAME_OF_IDEA_PROJECT;

/**
 * 执行节点
 */
public class ShowConfigJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject config = nodeJsonDefine.getJSONObject("config");

        Project project = (Project) input.getNodeContext().getAttribute(FIELD_NAME_OF_IDEA_PROJECT);

        UserIdeManager.getUserIdeInterface().showNodeConfig(config, project);

        String configField = nodeJsonDefine.getString("configField");

        if (StrUtil.isBlank(configField)) {
            configField = "config";
        }

        config = UserIdeManager.getUserIdeInterface().getNodeConfig();

        input.getNodeContext().setAttribute(configField, config);

        output.set(configField, config);
    }
}
