package fit.lang.plugin.json.ide;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ShowConfigJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONObject config = nodeJsonDefine.getJSONObject("config");

        Project project = (Project) input.getNodeContext().getAttribute("ideaProject");

        UserIdeManager.getUserIdeInterface().showNodeConfig(config, project);

    }
}
