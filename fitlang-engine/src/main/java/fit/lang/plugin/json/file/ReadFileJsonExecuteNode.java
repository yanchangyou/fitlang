package fit.lang.plugin.json.file;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.CharsetUtil;
import cn.hutool.core.util.StrUtil;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ReadFileJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        //限定只能操作指定的路径
        String workspaceDir = parseStringField("workspaceDir", input);

        String path = parseStringField("filePath", input);
        String charset = parseStringField("charset", input);

        if (StrUtil.isBlank(workspaceDir)) {
            throw new ExecuteNodeException("readFile workspaceDir param is required!");
        }

        if (StrUtil.isBlank(path)) {
            throw new ExecuteNodeException("readFile filePath param is required!");
        }

        //避免遍历父目录
        path = path.replace("..", "");

        if (StrUtil.isBlank(charset)) {
            charset = "UTF-8";
        }

        String filePath;
        //兼容收尾斜杠写法
        if (workspaceDir.endsWith("/") && path.startsWith("/")) {
            filePath = workspaceDir.concat(path.substring(1));
        } else if (!workspaceDir.endsWith("/") && !path.startsWith("/")) {
            filePath = workspaceDir.concat("/").concat(path);
        } else {
            filePath = workspaceDir.concat(path);
        }

        String content = FileUtil.readString(filePath, CharsetUtil.charset(charset));
        output.set("content", content);
        output.set("absolutePath", filePath);
    }
}
