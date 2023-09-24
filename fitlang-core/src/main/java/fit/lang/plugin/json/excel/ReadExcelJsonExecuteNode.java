package fit.lang.plugin.json.excel;

import cn.hutool.core.util.StrUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.List;
import java.util.Map;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.buildFilePath;
import static fit.lang.plugin.json.excel.util.JxlExcelUtil.readExcel;
//import static fit.lang.plugin.json.excel.util.EasyExcelUtil.readExcel;

/**
 * 执行节点
 */
public class ReadExcelJsonExecuteNode extends JsonExecuteNode {

    public static final String FIELD_NAME_OF_OUTPUT_LIST_FIELD = "outputListField";

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        Integer titleIndex = nodeJsonDefine.getInteger("titleIndex");
        String listField = nodeJsonDefine.getString(FIELD_NAME_OF_OUTPUT_LIST_FIELD);
        if (StrUtil.isBlank(listField)) {
            listField = "list";
        }

        String path = parseStringField("path", input);
        String sheetName = parseStringField("sheetName", input);

        path = buildFilePath(path, "readExcel");

        List<Map<String, String>> list;
        try {
            list = readExcel(path, sheetName, titleIndex);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        output.set(listField, list);
        output.set("sheetName", sheetName);
        output.set("path", path);
        output.set("titleIndex", titleIndex);
    }

}
