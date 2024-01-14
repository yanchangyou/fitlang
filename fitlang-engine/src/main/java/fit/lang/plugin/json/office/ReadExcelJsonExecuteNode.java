package fit.lang.plugin.json.office;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class ReadExcelJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String file = nodeJsonDefine.getString("file");

        String sheetName = nodeJsonDefine.getString("sheetName");

        JSONObject excel;
        try {
            excel = NodeExcelUtil.readExcel(file, sheetName);
        } catch (Exception e) {
            throw new ExecuteNodeException("read excel error: ", e);
        }

        output.setData(excel);
    }

}
