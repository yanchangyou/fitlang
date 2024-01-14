package fit.lang.plugin.json.ide;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.File;
import java.util.List;

/**
 * 执行节点
 */
public class ChooseFileJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        Boolean isMultiple = Boolean.TRUE.equals(nodeJsonDefine.getBoolean("isMultiple"));
        String fileField = parseStringField("fileField", input);
        if (StrUtil.isBlank(fileField)) {
            fileField = "file";
        }

        JSONObject config = new JSONObject(1);
        config.put("isMultiple", isMultiple);

        List<File> fileList = UserIdeManager.getUserIdeInterface().chooseFiles(config);

        JSONArray pathList = new JSONArray();
        for (File file : fileList) {
            pathList.add(file.getAbsolutePath());
        }
        output.set(fileField, pathList);
        if (!isMultiple) {
            if (pathList.isEmpty()) {
                output.set(fileField, null);
            } else {
                output.set(fileField, pathList.get(0));
            }
        }
    }
}
