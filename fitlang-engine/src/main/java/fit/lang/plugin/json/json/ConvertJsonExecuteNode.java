package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONPath;
import fit.lang.ExecuteNodeException;
import fit.lang.define.ExecuteNodeBuildable;
import fit.lang.define.ExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.List;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public class ConvertJsonExecuteNode extends JsonExecuteNode implements ExecuteNodeBuildable {


    /**
     * 表达式
     */
    JSONObject express = new JSONObject();

    /**
     * 值映射
     */
    JSONObject valueMapping = new JSONObject();

    public JSONObject getExpress() {
        return express;
    }

    public void setExpress(JSONObject express) {
        this.express = express;
    }

    public JSONObject getValueMapping() {
        return valueMapping;
    }

    public void setValueMapping(JSONObject valueMapping) {
        this.valueMapping = valueMapping;
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        if (nodeJsonDefine != null) {
            Boolean isMixMode = nodeJsonDefine.getBoolean("isMixMode");
            if (Boolean.TRUE.equals(isMixMode)) {
                output.setData(input.getData().clone());
            }
        }

        convert(input.getData(), output.getData());
    }

    private void convert(JSONObject inputData, JSONObject outputData) {
        for (String to : express.keySet()) {

            Object from = express.get(to);

            Object value = from;

            //变量
            if (from instanceof String) {
                String fromString = (String) from;
                if (fromString.startsWith("${") && fromString.endsWith("}")) {
                    String frommInnerExpress = getInnerExpress(fromString);
                    //处理数组
                    if (frommInnerExpress.contains("[]")) {
                        convertArray(inputData, outputData, frommInnerExpress, to);
                        continue;
                    } else {
                        value = JSONPath.eval(inputData, frommInnerExpress);
                        value = valueMapping(to, value);
                    }
                }
            }
            JSONPath.set(outputData, to, value);
        }
    }

    private void convertArray(JSONObject inputData, JSONObject outputData, String from, String to) {
        Object value;
        String[] fromParts = from.split("\\[]");
        String[] toParts = to.split("\\[]");
        List array = (List) JSONPath.eval(inputData, fromParts[0]);

        if (array == null) {
            JSONPath.set(outputData, to.split("\\[]")[0], (Object) null);
            return;
        }

        int[] length = new int[fromParts.length - 1];
        int[] index = new int[fromParts.length - 1];

        index[index.length - 1] = -1;

        buildLength(inputData, fromParts, length, index, index.length - 1);

        while (next(inputData, fromParts, length, index, index.length - 1)) {
            String fromPath = buildPath(fromParts, index);
            String toPath = buildPath(toParts, index);
            value = JSONPath.eval(inputData, fromPath);
            value = valueMapping(toPath, value);
            JSONPath.set(outputData, toPath, value);
        }
    }

    String buildPath(String[] parts, int[] index) {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < parts.length; i++) {
            if (i > 0) {
                builder.append("[").append(index[i - 1]).append("]");
            }
            builder.append(parts[i]);
        }
        return builder.toString();
    }

    void buildLength(JSONObject root, String[] pathParts, int[] length, int[] index, int level) {
        StringBuilder jsonPath = new StringBuilder();
        for (int i = 0; i < level + 1; i++) {
            if (i > 0) jsonPath.append("[").append(index[i - 1]).append("]");
            jsonPath.append(pathParts[i]);
            List array = (List) JSONPath.eval(root, jsonPath.toString());
            if (array != null) {
                length[i] = array.size();
            } else {
                length[i] = 0;
            }
        }
    }

    boolean next(JSONObject root, String[] pathParts, int[] length, int[] index, int level) {
        index[level]++;
        if (index[level] >= length[level]) {

            if (level == 0) {
                return false;
            } else {
                index[level - 1]++;
                if (index[level - 1] >= length[level - 1]) {
                    return next(root, pathParts, length, index, level - 1);
                }
                for (int i = level; i < index.length; i++) {
                    buildLength(root, pathParts, length, index, i);
                    index[i] = 0;
                }
                return true;
            }
        }
        return true;
    }

    String getInnerExpress(String express) {
        return express.substring(2, express.length() - 1);
    }

    Object valueMapping(String path, Object value) {

        String field = path;
        if (path.contains(".")) {
            String[] parts = path.split("\\.");
            field = parts[parts.length - 1];
        }
        String valueString = value == null ? "null" : value.toString();
        if (valueMapping != null && valueMapping.containsKey(field)) {
            JSONObject mapping = valueMapping.getJSONObject(field);
            if (mapping != null && mapping.containsKey(valueString)) {
                return mapping.get(valueString);
            }
        }
        return value;
    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {

        setExpress(getJsonData(executeNodeData).getJSONObject("express"));

        if (express == null) {
            throw new ExecuteNodeException("convert express field is required!");
        }
        setValueMapping(getJsonData(executeNodeData).getJSONObject("valueMapping"));
    }
}