package fit.lang.plugin.json.git;

import cn.hutool.core.util.RuntimeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.File;

/**
 * 执行节点
 */
public class GitPullJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String gitPath = parseStringField("gitPath", input);

        if (gitPath == null) {
            gitPath = ".";
        }
        String message;
        File file = new File(gitPath);
        if (file.exists()) {
            message = RuntimeUtil.execForStr("git -C ".concat(gitPath).concat(" pull"));
            output.set("gitPath", file.getAbsolutePath());
        } else {
            message = "gitPath not exited: ".concat(gitPath);
        }

        output.set("message", message);

    }
}
