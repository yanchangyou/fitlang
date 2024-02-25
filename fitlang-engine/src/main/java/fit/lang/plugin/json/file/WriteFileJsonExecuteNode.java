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
        String charset = nodeJsonDefine.getString("charset");
        String contentField = parseStringField("contentField", input);

        if (StrUtil.isBlank(workspaceDir)) {
//            throw new ExecuteNodeException("writeFile workspaceDir param is required!");
            workspaceDir = "";
        }

        if (StrUtil.isBlank(path)) {
            throw new ExecuteNodeException("writeFile filePath param is required!");
        }

        if (StrUtil.isBlank(charset)) {
            charset = "UTF-8";
        }

        String filePath = joinFilePath(workspaceDir, path);

        String text;

        Object content;

        if (StrUtil.isBlank(contentField)) {
            content = input.getData();
        } else {
            content = input.get(contentField);
        }

        Boolean format = nodeJsonDefine.getBoolean("format");
        if (Boolean.TRUE.equals(format) && content instanceof JSONObject) {
            text = toJsonTextWithFormat((JSONObject) content);
        } else {
            text = content.toString();
        }
        File file = FileUtil.writeString(text, filePath, CharsetUtil.charset(charset));
        output.set("absolutePath", file.getAbsoluteFile());
    }

}
