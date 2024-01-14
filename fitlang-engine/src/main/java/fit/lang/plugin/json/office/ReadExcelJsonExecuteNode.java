package fit.lang.plugin.json.office;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.office.NodeExcelUtil.readExcel;

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

}
