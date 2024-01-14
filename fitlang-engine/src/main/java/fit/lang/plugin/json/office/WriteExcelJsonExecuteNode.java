package fit.lang.plugin.json.office;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import jxl.Workbook;
import jxl.write.Label;
import jxl.write.WritableCell;
import jxl.write.WritableSheet;
import jxl.write.WritableWorkbook;

import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;

/**
 * 执行节点
 */
public class WriteExcelJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String path = parseStringField("path", input);

        String sheetName = nodeJsonDefine.getString("sheetName");
        String arrayField = nodeJsonDefine.getString("arrayField");
        if (StrUtil.isBlank(arrayField)) {
            arrayField = "list";
        }

        JSONArray rows = input.getJsonArray(arrayField);

        JSONObject result = null;
        try {
            result = writeExcel(path, sheetName, rows);
        } catch (Exception e) {
            throw new ExecuteNodeException("write excel error: ", e);
        }

        output.set("result", result);
    }

    private JSONObject writeExcel(String path, String sheetName, JSONArray rows) throws Exception {

        OutputStream outputStream = Files.newOutputStream(Paths.get(path));
        WritableWorkbook rwb = Workbook.createWorkbook(outputStream);

        JSONObject result = new JSONObject();
        int nextSheetNum = rwb.getNumberOfSheets() + 1;

        if (StrUtil.isBlank(sheetName)) {
            sheetName = "Sheet" + nextSheetNum;
        }
        WritableSheet sheet = rwb.createSheet(sheetName, nextSheetNum);

        int rowIndex = 0;
        for (Object row : rows) {
            JSONObject rowInfo = (JSONObject) row;
            int columnIndex = 0;
            for (Map.Entry<String, Object> rowEntry : rowInfo.entrySet()) {
                WritableCell cell = new Label(columnIndex++, rowIndex, rowEntry.getValue().toString());
                sheet.addCell(cell);
            }
            rowIndex++;
        }
        rwb.write();
        rwb.close();

        return result;
    }
}