package fit.lang.plugin.json.file;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.CharsetUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.File;
import java.util.List;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.joinFilePath;

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
//            throw new ExecuteNodeException("writeFile workspaceDir param is required!");
            workspaceDir = "";
        }

        if (StrUtil.isBlank(path)) {
            throw new ExecuteNodeException("readFile filePath param is required!");
        }

        if (StrUtil.isBlank(charset)) {
            charset = "UTF-8";
        }

        String filePath = joinFilePath(workspaceDir, path);

        File file = new File(filePath);
        if (!file.exists()) {
            throw new ExecuteNodeException("file not found: " + filePath);
        }

        if (file.isFile()) {
            output.set("isFile", true);
            String content = FileUtil.readString(filePath, CharsetUtil.charset(charset));
            output.set("content", content);
        } else {
            output.set("isFile", false);
            List<String> fileNames = FileUtil.listFileNames(filePath);
            output.set("files", fileNames);
            File[] files = file.listFiles();
            JSONArray fileList = new JSONArray();
            for (File subFile : files) {
                JSONObject subFileJson = new JSONObject();
                subFileJson.put("name", subFile.getName());
                subFileJson.put("path", subFile.getAbsoluteFile());
                subFileJson.put("isFile", subFile.isFile());
                fileList.add(subFileJson);
            }
            output.set("files", fileList);
        }

        output.set("absolutePath", file.getAbsoluteFile());
    }
}
