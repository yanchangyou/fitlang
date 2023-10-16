package fit.lang.plugin.json.file;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.CharsetUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.File;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.joinFilePath;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

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

        if (StrUtil.isBlank(charset)) {
            charset = "UTF-8";
        }

        String filePath = joinFilePath(workspaceDir, path);

        Object content = input.get(contentField);
        if (content == null) {
            throw new ExecuteNodeException("write content is null");
        }
        String text;
        Boolean format = nodeJsonDefine.getBoolean("format");
        if (format != null && format && content instanceof JSONObject) {
            text = toJsonTextWithFormat((JSONObject) content);
        } else {
            text = content.toString();
        }
        File file = FileUtil.writeString(text, filePath, CharsetUtil.charset(charset));
        output.set("absolutePath", file.getAbsoluteFile());
    }

}
