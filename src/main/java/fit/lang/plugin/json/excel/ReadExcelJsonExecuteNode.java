package fit.lang.plugin.json.excel;

import fit.lang.plugin.json.ExcelUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

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

        List<Map<String, String>> list = ExcelUtil.readExcel(path, sheetName, headerIndex);

        output.set("sheetData", list);
    }
}
