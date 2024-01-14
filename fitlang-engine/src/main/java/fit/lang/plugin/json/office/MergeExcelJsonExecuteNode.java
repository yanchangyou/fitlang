package fit.lang.plugin.json.office;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.File;
import java.util.List;

import static fit.lang.ExecuteNodeUtil.getNow;
import static fit.lang.plugin.json.office.NodeExcelUtil.readExcel;
import static fit.lang.plugin.json.office.NodeExcelUtil.writeExcel;

/**
 * 执行节点
 */
public class MergeExcelJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        List<String> inputFiles = parseStringArray("inputFiles", input);

        if (inputFiles == null || inputFiles.isEmpty()) {
            output.set("inputFiles", inputFiles);
            return;
        }

        String outputFile = parseStringField("outputFile", input);
        if (StrUtil.isBlank(outputFile)) {
            outputFile = new File(inputFiles.get(0)).getParent() + File.separator + "merge-" + getNow("yyyyMMddHHmmss") + ".xls";
        }

        String sheetName = nodeJsonDefine.getString("sheetName");

        JSONArray rows = new JSONArray();
        try {
            for (String inputFile : inputFiles) {
                JSONObject sheet = readExcel(inputFile, sheetName);
                if (sheet == null) {
                    continue;
                }
                JSONArray inputRows = sheet.getJSONArray("rows");
                if (inputRows != null) {
                    rows.addAll(inputRows);
                }
            }
            outputFile = outputFile.replace("[", "").replace("]", "");
            writeExcel(outputFile, sheetName, rows, true);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        output.set("inputFiles", JSON.toJSON(inputFiles));
        output.set("outputFile", outputFile);
        output.set("sheetName", sheetName);
        output.set("rows", rows);
    }

}
