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

        String path = parseStringField("path", input);

        if (path == null) {
            path = ".";
        }
        String message = parseStringField("message", input);
        File file = new File(path);
        if (file.exists()) {
            message = RuntimeUtil.execForStr("git pull ".concat(path));
            output.set("path", file.getAbsolutePath());
        } else {
            message = "path not exited: ".concat(path);
        }

        output.set("message", message);

    }
}
