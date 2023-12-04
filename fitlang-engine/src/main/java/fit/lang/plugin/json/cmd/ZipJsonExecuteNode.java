package fit.lang.plugin.json.cmd;

import cn.hutool.core.util.StrUtil;
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

        String path = parseStringField("path", input);

        if (StrUtil.isBlank(path)) {
            output.set("message", "path is empty!");
        } else {
            String zipFile = path.concat(".zip");
            File file = ZipUtil.zip(path, zipFile, true);
            output.set("zipPath", file.getAbsoluteFile());
        }
    }
}
