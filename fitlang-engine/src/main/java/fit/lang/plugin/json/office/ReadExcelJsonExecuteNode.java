package fit.lang.plugin.json.office;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import jxl.Sheet;
import jxl.Workbook;
import jxl.read.biff.BiffException;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * 执行节点
 */
public class ReadExcelJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String path = nodeJsonDefine.getString("path");

        JSONObject excel = null;
        try {
            excel = readExcel(path);
        } catch (Exception e) {
            throw new ExecuteNodeException("read excel error: ", e);
        }

        output.setData(excel);
    }

    JSONObject readExcel(String path) throws IOException, BiffException {
        InputStream is = Files.newInputStream(Paths.get(path));
        jxl.Workbook rwb = Workbook.getWorkbook(is);

        JSONObject excel = new JSONObject();
        excel.put("path", path);

        JSONArray sheetsData = new JSONArray();
        Sheet[] sheets = rwb.getSheets();
        for (Sheet sheet : sheets) {
            String sheetName = sheet.getName();
            JSONArray rows = new JSONArray();
            int rowNum = sheet.getRows();
            for (int i = 0; i < rowNum; i++) {
                JSONObject row = new JSONObject();

                int column = sheet.getRow(i).length;
                for (int j = 0; j < column; j++) {
                    row.put("column" + j, sheet.getCell(j, i).getContents());
                }
                rows.add(row);
            }
            JSONObject sheetData = new JSONObject();
            sheetData.put("name", sheetName);
            sheetData.put("rows", rows);
            sheetsData.add(sheetData);
        }

        excel.put("sheets", sheetsData);
        return excel;
    }
}
