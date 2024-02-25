package fit.lang.plugin.json.office;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.office.NodeExcelUtil.writeExcel;

/**
 * 执行节点
 */
public class WriteExcelJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String file = parseStringField("file", input);
        JSONObject header = nodeJsonDefine.getJSONObject("header");

        String sheetName = nodeJsonDefine.getString("sheetName");
        String arrayField = nodeJsonDefine.getString("arrayField");
        boolean isAppend = Boolean.TRUE.equals(nodeJsonDefine.getBoolean("isAppend"));

        if (StrUtil.isBlank(arrayField)) {
            arrayField = "list";
        }

        JSONArray rows = input.getJsonArray(arrayField);

        JSONObject result = null;
        try {
            result = writeExcel(file, sheetName, rows, isAppend, header);
        } catch (Exception e) {
            throw new ExecuteNodeException("write excel error: ", e);
        }

        output.set("arrayField", arrayField);
        output.set("result", result);

    }

}