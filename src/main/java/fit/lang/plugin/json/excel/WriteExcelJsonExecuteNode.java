package fit.lang.plugin.json.excel;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.excel.util.EasyExcelUtil;

import java.io.IOException;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.buildFilePath;

/**
 * 执行节点
 */
public class WriteExcelJsonExecuteNode extends JsonExecuteNode {

    public static final String FIELD_NAME_OF_INPUT_LIST_FIELD = "inputListField";

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String path = nodeJsonDefine.getString("path");
        Integer defaultWidth = nodeJsonDefine.getInteger("defaultWidth");
        String listField = nodeJsonDefine.getString(FIELD_NAME_OF_INPUT_LIST_FIELD);
        JSONObject titleConfig = nodeJsonDefine.getJSONObject("titleConfig");

        String sheetName = nodeJsonDefine.getString("sheetName");
        if (StrUtil.isBlank(sheetName)) {
            sheetName = input.getString("sheetName");
        }

        if (StrUtil.isBlank(listField)) {
            listField = "list";
        }

        if (StrUtil.isBlank(path)) {
            path = input.getString("path");
        }

        //支持表达式
        path = (String) ExpressUtil.eval(path, input.getInputParamAndContextParam());

        path = buildFilePath(path);

        JSONArray list = input.getJsonArray(listField);

        try {
            EasyExcelUtil.writeExcel(sheetName, list, titleConfig, defaultWidth, path);
        } catch (IOException e) {
            throw new ExecuteNodeException(e.getMessage());
        }

        output.set(listField, list);
    }
}
