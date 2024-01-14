package fit.lang.plugin.json.office;

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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 执行节点
 */
public class ReadExcelJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String path = nodeJsonDefine.getString("path");
        String sheetName = nodeJsonDefine.getString("sheetName");
        Integer headerIndex = nodeJsonDefine.getInteger("headerIndex");

        List<Map<String, String>> list;
        try {
            list = readExcel(path, sheetName, headerIndex);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        output.set("sheetData", list);
    }

    List<Map<String, String>> readExcel(String path, String sheetName, Integer headerIndex) throws IOException, BiffException {
        InputStream is = Files.newInputStream(Paths.get(path));
        jxl.Workbook rwb = Workbook.getWorkbook(is);

        if (headerIndex == null) {
            headerIndex = 0;
        }
        Sheet sheet = rwb.getSheet(0);
        if (sheetName != null) {
            sheet = rwb.getSheet(sheetName);
        }
        List<Map<String, String>> list = new ArrayList<>();
        int rows = sheet.getRows();
        for (int i = headerIndex; i < rows; i++) {
            Map<String, String> row = new HashMap<>();
            int column = sheet.getRow(i).length;
            for (int j = 0; j < column; j++) {
                row.put(j + "", sheet.getCell(j, i).getContents());
            }
            list.add(row);
        }

        return list;
    }
}
