package fit.lang.plugin.json.excel.util;

import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.event.AnalysisEventListener;
import com.alibaba.excel.metadata.Head;
import com.alibaba.excel.read.builder.ExcelReaderBuilder;
import com.alibaba.excel.util.BooleanUtils;
import com.alibaba.excel.write.handler.context.CellWriteHandlerContext;
import com.alibaba.excel.write.handler.impl.FillStyleCellWriteHandler;
import com.alibaba.excel.write.metadata.style.WriteCellStyle;
import com.alibaba.excel.write.metadata.style.WriteFont;
import com.alibaba.excel.write.style.HorizontalCellStyleStrategy;
import com.alibaba.excel.write.style.column.SimpleColumnWidthStyleStrategy;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import org.apache.poi.ss.usermodel.*;

import java.io.IOException;
import java.util.*;

import static org.apache.poi.ss.usermodel.BorderStyle.THIN;

public class EasyExcelUtil {

    /**
     * 写Excel
     *
     * @param list
     * @param titleConfig
     * @param defaultWidth
     * @param path
     * @throws IOException
     */
    public static void writeExcel(String sheetName, JSONArray list, JSONObject titleConfig, Integer defaultWidth, String path) throws IOException {
        List<List<String>> data = new ArrayList<>();
        // 设置列头
        List<List<String>> titles = new ArrayList<>();
        List<String> headers = new ArrayList<>();
        Map<Integer, Integer> columnWidthMap = new HashMap<>();

        if (titleConfig == null) {
            titleConfig = new JSONObject();
        }

        Integer columnIndex = 0;

        if (list.isEmpty()) {
            for (Map.Entry<String, Object> entry : titleConfig.entrySet()) {
                JSONObject config = (JSONObject) entry.getValue();
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

            JSONObject row = (JSONObject) list.get(0);

            if (titleConfig.isEmpty()) {
                for (Map.Entry<String, Object> entry : row.entrySet()) {
                    String title = entry.getKey();
                    titles.add(Collections.singletonList(title));
                    headers.add(entry.getKey());
                }
            } else {
                for (Map.Entry<String, Object> entry : titleConfig.entrySet()) {
                    JSONObject config = (JSONObject) entry.getValue();
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

            for (Object item : list) {
                List rowData = new ArrayList();
                for (String head : headers) {
                    String value = ((JSONObject) item).getString(head);
                    rowData.add(value);
                }
                data.add(rowData);
            }
        }

        writeExcel(sheetName, titles, data, defaultWidth, columnWidthMap, path);
    }

    /**
     * 写excel到输出流中
     *
     * @param sheetName
     * @param titles
     * @param data
     * @param defaultWidth
     * @param columnWidthMap
     * @param path
     * @throws IOException
     */
    private static void writeExcel(String sheetName, List<List<String>> titles, List<List<String>> data, Integer defaultWidth, Map<Integer, Integer> columnWidthMap, String path) throws IOException {

        if (defaultWidth == null) {
            defaultWidth = 30;
        }
        if (sheetName == null) {
            sheetName = "data";
        }
        HorizontalCellStyleStrategy horizontalCellStyleStrategy = exportExcelConfig();
        Integer finalDefaultWidth = defaultWidth;
        EasyExcel.write(path)
//                .registerWriteHandler(horizontalCellStyleStrategy)

                .registerWriteHandler(new SimpleColumnWidthStyleStrategy(finalDefaultWidth) {
                    protected Integer columnWidth(Head head, Integer columnIndex) {
                        return columnWidthMap.get(columnIndex) == null ? finalDefaultWidth : (columnWidthMap.get(columnIndex) / 6);
                    }
                })
                .registerWriteHandler(new FillStyleCellWriteHandler() {

                    Map<String, CellStyle> styleCache = new HashMap();

                    @Override
                    public void afterCellDispose(CellWriteHandlerContext context) {
                        if (BooleanUtils.isTrue(context.getIgnoreFillStyle())) {
                            return;
                        }
                        CellStyle cellStyle = context.getCell().getCellStyle();
                        String stringValue = context.getCell().getStringCellValue();
                        Workbook workbook = context.getWriteWorkbookHolder().getWorkbook();

                        if ("true".equals(stringValue)) {
                            CellStyle newCellStyle = styleCache.get("true");
                            if (newCellStyle == null) {
                                Font writeFont = workbook.createFont();
                                newCellStyle = workbook.createCellStyle();
                                newCellStyle.setFont(writeFont);
                                writeFont.setColor(IndexedColors.GREEN.getIndex());
                                styleCache.put("true", newCellStyle);
                            }
                            context.getCell().setCellStyle(newCellStyle);
                        } else if ("false".equals(stringValue)) {
                            CellStyle newCellStyle = styleCache.get("false");
                            if (newCellStyle == null) {
                                Font writeFont = workbook.createFont();
                                newCellStyle = workbook.createCellStyle();
                                newCellStyle.setFont(writeFont);
                                writeFont.setColor(IndexedColors.RED.getIndex());
                                styleCache.put("false", newCellStyle);
                            }
                            context.getCell().setCellStyle(newCellStyle);
                        }
                    }
                })
                .sheet(sheetName).table().head(titles).doWrite(data);
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

    public static List<Map<String, String>> readExcel(String path, String sheetName, Integer titleIndex) {
        List<Map<Integer, String>> list = new ArrayList<>();
        Map<Integer, String> excelHeader = new LinkedHashMap<>();
        if (titleIndex == null) {
            titleIndex = 1;
        }
        if (titleIndex < 1) {
            throw new ExecuteNodeException("excel titleIndex must be great than 0, but found: " + titleIndex);
        }

        ExcelReaderBuilder excelReaderBuilder = EasyExcel.read(path, new ExcelService(list, excelHeader));
        if (sheetName != null) {
            excelReaderBuilder.sheet(sheetName).headRowNumber(titleIndex).doReadSync();
        } else {
            excelReaderBuilder.sheet(0).headRowNumber(titleIndex).doReadSync();
        }

        return convertExcelData(list, excelHeader);
    }

    static List<Map<String, String>> convertExcelData(List<Map<Integer, String>> list, Map<Integer, String> excelHeader) {
        if (excelHeader == null || excelHeader.isEmpty()) {
            throw new ExecuteNodeException("excel must be have header!(config titleIndex field)");
        }
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
