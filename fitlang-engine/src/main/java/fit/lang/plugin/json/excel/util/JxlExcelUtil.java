package fit.lang.plugin.json.excel.util;

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

public class JxlExcelUtil {

    public static List<Map<String, String>> readExcel(String path, String sheetName, Integer titleIndex) throws IOException, BiffException {
        InputStream is = Files.newInputStream(Paths.get(path));
        jxl.Workbook rwb = Workbook.getWorkbook(is);

        if (titleIndex == null) {
            titleIndex = 0;
        } else {
            //外部约定从1开始，jxl从0开始
            titleIndex = titleIndex - 1;
        }
        Sheet sheet = rwb.getSheet(0);
        if (sheetName != null) {
            sheet = rwb.getSheet(sheetName);
        }
        List<Map<String, String>> list = new ArrayList<>();
        int rows = sheet.getRows();
        for (int i = titleIndex; i < rows; i++) {
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
