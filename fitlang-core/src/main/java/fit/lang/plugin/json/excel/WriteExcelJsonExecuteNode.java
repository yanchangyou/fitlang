package fit.lang.plugin.json.excel;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.IOException;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.buildFilePath;
import static fit.lang.plugin.json.excel.util.JxlExcelUtil.writeExcel;

/**
 * 执行节点
 */
public class WriteExcelJsonExecuteNode extends JsonExecuteNode {

    public static final String FIELD_NAME_OF_INPUT_LIST_FIELD = "inputListField";

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        Integer defaultWidth = nodeJsonDefine.getInteger("defaultWidth");
        String listField = nodeJsonDefine.getString(FIELD_NAME_OF_INPUT_LIST_FIELD);
        JSONObject titleConfig = nodeJsonDefine.getJSONObject("titleConfig");

        if (StrUtil.isBlank(listField)) {
            listField = "list";
        }

        String sheetName = parseStringField("sheetName", input);
        String path = parseStringField("path", input);
        path = buildFilePath(path, "readExcel");

        JSONArray list = input.getJsonArray(listField);

        try {
            writeExcel(list, titleConfig, defaultWidth, path, sheetName);
        } catch (IOException e) {
            throw new ExecuteNodeException(e.getMessage());
        }

        output.set(listField, list);
    }
}
