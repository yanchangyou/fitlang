package fit.lang.plugin.json.excel;

import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.List;
import java.util.Map;

import static fit.lang.plugin.json.excel.util.EasyExcelUtil.readExcel;

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

}
