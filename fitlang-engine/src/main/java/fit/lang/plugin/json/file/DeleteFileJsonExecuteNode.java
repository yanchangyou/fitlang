package fit.lang.plugin.json.file;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.CharsetUtil;
import cn.hutool.core.util.StrUtil;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.File;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.joinFilePath;

/**
 * 执行节点
 */
public class DeleteFileJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        //限定只能操作指定的路径
        String workspaceDir = parseStringField("workspaceDir", input);

        String path = parseStringField("filePath", input);

        if (StrUtil.isBlank(workspaceDir)) {
//            throw new ExecuteNodeException("writeFile workspaceDir param is required!");
            workspaceDir = "";
        }

        if (StrUtil.isBlank(path)) {
            throw new ExecuteNodeException("writeFile filePath param is required!");
        }

        String filePath = joinFilePath(workspaceDir, path);

        if (!new File(filePath).exists()) {
            throw new ExecuteNodeException("file not found: " + filePath);
        }

        boolean success = FileUtil.del(filePath);
        output.set("success", success);
        output.set("absolutePath", filePath);
    }

}
