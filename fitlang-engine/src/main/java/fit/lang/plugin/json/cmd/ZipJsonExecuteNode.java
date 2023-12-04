package fit.lang.plugin.json.cmd;

import cn.hutool.core.util.ZipUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.File;

/**
 * 执行节点
 */
public class ZipJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String filePath = parseStringField("filePath", input);

        if (filePath != null) {
            String zipFile = filePath.substring(0, filePath.lastIndexOf(".")).concat(".zip");
            File file = ZipUtil.zip(filePath, zipFile, true);
            output.set("zipPath", file.getAbsoluteFile());
        } else {
            output.set("message", "filePath is null!");
        }
    }
}
