package fit.lang.plugin.json.office;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import jxl.Sheet;
import jxl.Workbook;
import jxl.write.Label;
import jxl.write.WritableCell;
import jxl.write.WritableSheet;
import jxl.write.WritableWorkbook;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;

public class NodeExcelUtil {

    static JSONObject readExcelAllSheet(String path) throws Exception {
        File file = new File(path);
        if (!file.exists()) {
            return null;
        }
        InputStream is = new FileInputStream(file);
        jxl.Workbook rwb = Workbook.getWorkbook(is);

        JSONObject excel = new JSONObject();
        excel.put("path", path);

        JSONArray sheetList = new JSONArray();
        Sheet[] sheets = rwb.getSheets();
        for (Sheet sheet : sheets) {
            JSONObject sheetData = readSheet(sheet);
            sheetList.add(sheetData);
        }
        rwb.close();

        excel.put("sheets", sheetList);
        return excel;
    }

    static JSONObject readExcel(String path, String sheetName) throws Exception {
        JSONObject excel = readExcelAllSheet(path);
        if (excel == null) {
            return null;
        }

        JSONArray sheets = excel.getJSONArray("sheets");
        if (sheetName == null) {
            if (!sheets.isEmpty()) {
                return sheets.getJSONObject(0);
            } else {
                return null;
            }
        }

        for (Object sheetObject : sheets) {
            JSONObject sheet = (JSONObject) sheetObject;
            if (sheetName.equals(sheet.getString("name"))) {
                return sheet;
            }
        }

        return null;
    }


    static JSONObject readSheet(Sheet sheet) {
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
        return sheetData;
    }


    static JSONObject writeExcel(String path, String sheetName, JSONArray rows, boolean isAppend) throws Exception {

        File outFile = new File(path);
        boolean canWrite = outFile.canWrite();
        if (!canWrite) {
            JSONObject result = new JSONObject();
            result.put("error", "can not write file: " + outFile);
            return result;
        }
        if (!outFile.exists()) {
            try {
                outFile.createNewFile();
            } catch (IOException e) {
                JSONObject result = new JSONObject();
                result.put("error", "can not create file: " + outFile + ", error: " + e.getMessage());
                return result;
            }
        }

        if (StrUtil.isBlank(sheetName)) {
            sheetName = "Sheet1";
        }

        if (isAppend) {
            try {
                JSONObject sheetData = readExcel(path, sheetName);
                if (sheetData != null) {
                    JSONArray oldRows = sheetData.getJSONArray("rows");
                    oldRows.addAll(rows);
                    rows = oldRows;
                }
            } catch (Exception e) {
                //ignore
            }
        }

        OutputStream outputStream = Files.newOutputStream(Paths.get(path));
        WritableWorkbook rwb = Workbook.createWorkbook(outputStream);

        JSONObject result = new JSONObject();
        int nextSheetNum = rwb.getNumberOfSheets() + 1;

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

        result.put("path", path);
        result.put("sheetName", sheetName);
        result.put("rows", rows);
        result.put("isAppend", isAppend);

        return result;
    }

    static boolean isFirstSheet(String sheetName) {
        return "0".equals(sheetName);
    }
}
