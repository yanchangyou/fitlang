package fit.lang.plugin.json.cmd;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.core.util.ZipUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.File;
import java.util.Date;

import static cn.hutool.core.date.DatePattern.PURE_DATETIME_PATTERN;

/**
 * 执行节点
 */
public class UnzipJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String path = parseStringField("path", input);

        if (StrUtil.isBlank(path)) {
            output.set("message", "path is empty!");
        } else if (!path.endsWith(".zip")) {
            output.set("message", "path must be end with zip!");
        } else {
            String targetDir = path.substring(0, path.lastIndexOf(File.separator));
            if (new File(path.replace(".zip", "")).exists()) {
                targetDir = path.replace(".zip", "").concat(DateUtil.format(new Date(), PURE_DATETIME_PATTERN));
            }
            File file = ZipUtil.unzip(path, targetDir);
            output.set("path", path);
            output.set("unzipPath", file.getAbsoluteFile());
        }
    }
}
