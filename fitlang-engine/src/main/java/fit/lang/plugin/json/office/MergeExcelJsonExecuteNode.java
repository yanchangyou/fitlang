package fit.lang.plugin.json.office;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.List;

import static fit.lang.plugin.json.office.NodeExcelUtil.readExcel;
import static fit.lang.plugin.json.office.NodeExcelUtil.writeExcel;

/**
 * 执行节点
 */
public class MergeExcelJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        List<String> inputFiles = parseStringArray("inputFiles", input);

        String outputFile = nodeJsonDefine.getString("outputFile");

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
            writeExcel(outputFile, sheetName, rows, true);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        output.set("inputFiles", inputFiles);
        output.set("outputFile", outputFile);
        output.set("sheetName", sheetName);
        output.set("rows", rows);
    }
}
