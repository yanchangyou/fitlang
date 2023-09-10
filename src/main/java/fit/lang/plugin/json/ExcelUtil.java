package fit.lang.plugin.json;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.event.AnalysisEventListener;
import com.alibaba.excel.metadata.Head;
import com.alibaba.excel.read.builder.ExcelReaderBuilder;
import com.alibaba.excel.write.metadata.style.WriteCellStyle;
import com.alibaba.excel.write.metadata.style.WriteFont;
import com.alibaba.excel.write.style.HorizontalCellStyleStrategy;
import com.alibaba.excel.write.style.column.SimpleColumnWidthStyleStrategy;
import com.alibaba.fastjson2.JSONObject;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.VerticalAlignment;

import java.io.IOException;
import java.io.OutputStream;
import java.util.*;

import static org.apache.poi.ss.usermodel.BorderStyle.THIN;

public class ExcelUtil {
    /**
     * 写Excel
     *
     * @param list
     * @param fieldConfigMap
     * @param defaultWidth
     * @param outputStream
     * @throws IOException
     */
    public static void writeExcel(List<Map<String, String>> list, Map<String, JSONObject> fieldConfigMap, Integer defaultWidth, OutputStream outputStream) throws IOException {
        List<List<String>> data = new ArrayList<>();

        // 设置列头
        List<List<String>> titles = new ArrayList<>();
        List<String> headers = new ArrayList<>();
        Map<Integer, Integer> columnWidthMap = new HashMap<>();

        Integer columnIndex = 0;

        if (list.isEmpty()) {
            for (Map.Entry<String, JSONObject> entry : fieldConfigMap.entrySet()) {
                JSONObject config = entry.getValue();
                String title = config.getString("title");
                titles.add(Collections.singletonList(title));
                headers.add(entry.getKey());

                if (config.containsKey("labelWidth")) {
                    columnWidthMap.put(columnIndex, config.getIntValue("labelWidth"));
                }
                //隐藏的字段，默认宽度为0
                if (Boolean.TRUE.equals(config.getBoolean("hidden"))) {
                    columnWidthMap.put(columnIndex, 0);
                }
                columnIndex++;
            }
        } else {

            Map<String, String> row = list.get(0);

            if (fieldConfigMap.isEmpty()) {
                for (Map.Entry<String, String> entry : row.entrySet()) {
                    String title = entry.getKey();
                    titles.add(Collections.singletonList(title));
                    headers.add(entry.getKey());
                }
            } else {
                for (Map.Entry<String, JSONObject> entry : fieldConfigMap.entrySet()) {
                    JSONObject config = entry.getValue();
                    String title = config.getString("title");
                    titles.add(Collections.singletonList(title));
                    headers.add(entry.getKey());

                    if (config.containsKey("labelWidth")) {
                        columnWidthMap.put(columnIndex, config.getIntValue("labelWidth"));
                    }
                    //隐藏的字段，默认宽度为0
                    if (Boolean.TRUE.equals(config.getBoolean("hidden"))) {
                        columnWidthMap.put(columnIndex, 0);
                    }
                    columnIndex++;
                }
            }

        }

        writeExcel(titles, data, defaultWidth, columnWidthMap, outputStream);
    }

    /**
     * 写excel到输出流中
     *
     * @param titles
     * @param data
     * @param defaultWidth
     * @param columnWidthMap
     * @param outputStream
     * @throws IOException
     */
    private static void writeExcel(List<List<String>> titles, List<List<String>> data, Integer defaultWidth, Map<Integer, Integer> columnWidthMap, OutputStream outputStream) throws IOException {

        HorizontalCellStyleStrategy horizontalCellStyleStrategy = exportExcelConfig();
        EasyExcel.write(outputStream)
                .registerWriteHandler(horizontalCellStyleStrategy)
                .registerWriteHandler(new SimpleColumnWidthStyleStrategy(defaultWidth) {
                    protected Integer columnWidth(Head head, Integer columnIndex) {
                        return columnWidthMap.get(columnIndex) == null ? defaultWidth : (columnWidthMap.get(columnIndex) / 6);
                    }
                })
                .sheet().table().head(titles).doWrite(data);
    }

    /**
     * 样式设置
     */
    private static HorizontalCellStyleStrategy exportExcelConfig() {

        // 头部标题设置
        WriteCellStyle headStyle = new WriteCellStyle();
        // 设置背景色
        headStyle.setFillForegroundColor(IndexedColors.PALE_BLUE.getIndex());
        // 设置表头字体
        WriteFont headFont = new WriteFont();
        headFont.setFontHeightInPoints((short) 10);
        headFont.setBold(true);
        // 赋值字体
        headStyle.setWriteFont(headFont);
        // 设置表头水平居中
        headStyle.setHorizontalAlignment(HorizontalAlignment.CENTER);

        // 内容设置
        WriteCellStyle contentStyle = new WriteCellStyle();
        // 水平居中
        contentStyle.setHorizontalAlignment(HorizontalAlignment.LEFT);
        // 垂直居中
        contentStyle.setVerticalAlignment(VerticalAlignment.CENTER);
        // 边框样式
        contentStyle.setBorderLeft(THIN);
        contentStyle.setBorderTop(THIN);
        contentStyle.setBorderRight(THIN);
        contentStyle.setBorderBottom(THIN);
        return new HorizontalCellStyleStrategy(headStyle, contentStyle);
    }

    public static List<Map<String, String>> readExcel(String path, String sheetName, Integer headerIndex) {
        List<Map<Integer, String>> list = new ArrayList<>();
        Map<Integer, String> excelHeader = new LinkedHashMap<>();
        if (headerIndex == null) {
            headerIndex = 1;
        }

        ExcelReaderBuilder excelReaderBuilder = EasyExcel.read(path, new ExcelService(list, excelHeader));
        if (sheetName != null) {
            excelReaderBuilder.sheet(sheetName).headRowNumber(headerIndex).doReadSync();
        } else {
            excelReaderBuilder.sheet(0).headRowNumber(headerIndex).doReadSync();
        }

        return convertExcelData(list, excelHeader);
    }

    static List<Map<String, String>> convertExcelData(List<Map<Integer, String>> list, Map<Integer, String> excelHeader) {
        List<Map<String, String>> result = new ArrayList<>();
        for (Map<Integer, String> row : list) {
            Map<String, String> newRow = new HashMap<>();
            for (Map.Entry<Integer, String> entry : row.entrySet()) {
                newRow.put(excelHeader.get(entry.getKey()), entry.getValue());
            }
            result.add(newRow);
        }
        return result;
    }
}


class ExcelService extends AnalysisEventListener<Map<Integer, String>> {

    public ExcelService(List<Map<Integer, String>> list, Map<Integer, String> header) {
        this.list = list;
        this.header = header;
    }

    Map<Integer, String> header;

    List<Map<Integer, String>> list;

    @Override
    public void invoke(Map<Integer, String> data, AnalysisContext context) {
        list.add(data);
    }

    @Override
    public void invokeHeadMap(Map<Integer, String> headMap, AnalysisContext context) {

        header.putAll(headMap);
    }

    @Override
    public void doAfterAllAnalysed(AnalysisContext context) {
    }

}
