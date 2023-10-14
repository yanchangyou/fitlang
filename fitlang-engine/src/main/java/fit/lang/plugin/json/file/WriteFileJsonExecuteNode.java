package fit.lang.plugin.json.file;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.CharsetUtil;
import cn.hutool.core.util.StrUtil;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.joinFilePath;

/**
 * 执行节点
 */
public class WriteFileJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        //限定只能操作指定的路径
        String workspaceDir = parseStringField("workspaceDir", input);

        String path = parseStringField("filePath", input);
        String charset = parseStringField("charset", input);
        String contentField = parseStringField("contentField", input);

        if (StrUtil.isBlank(workspaceDir)) {
            throw new ExecuteNodeException("writeFile workspaceDir param is required!");
        }

        if (StrUtil.isBlank(path)) {
            throw new ExecuteNodeException("writeFile filePath param is required!");
        }

        if (StrUtil.isBlank(contentField)) {
            contentField = "content";
        }

        //避免遍历父目录
        path = path.replace("..", "");

        if (StrUtil.isBlank(charset)) {
            charset = "UTF-8";
        }

        String filePath = joinFilePath(workspaceDir, path);

        Object content = input.get(contentField);
        if (content == null) {
            throw new ExecuteNodeException("write content is null");
        }
        FileUtil.writeString(content.toString(), filePath, CharsetUtil.charset(charset));
        output.set("absolutePath", filePath);
    }

}
