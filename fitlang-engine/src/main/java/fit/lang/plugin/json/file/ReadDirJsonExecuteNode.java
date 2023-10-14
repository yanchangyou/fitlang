package fit.lang.plugin.json.file;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.List;

/**
 * 执行节点
 */
public class ReadDirJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        //限定只能操作指定的路径
        String workspaceDir = parseStringField("workspaceDir", input);

        String dirPath = parseStringField("dirPath", input);

        if (StrUtil.isBlank(workspaceDir)) {
            throw new ExecuteNodeException("readFile workspaceDir param is required!");
        }

        if (StrUtil.isBlank(dirPath)) {
            throw new ExecuteNodeException("readDir dirPath param is required!");
        }

        //避免遍历父目录
        dirPath = dirPath.replace("..", "");

        String filePath;
        //兼容收尾斜杠写法
        if (workspaceDir.endsWith("/") && dirPath.startsWith("/")) {
            filePath = workspaceDir.concat(dirPath.substring(1));
        } else if (!workspaceDir.endsWith("/") && !dirPath.startsWith("/")) {
            filePath = workspaceDir.concat("/").concat(dirPath);
        } else {
            filePath = workspaceDir.concat(dirPath);
        }

        List<String> fileNames = FileUtil.listFileNames(filePath);

        output.set("files", fileNames);
        output.set("absoluteDir", filePath);
    }
}
