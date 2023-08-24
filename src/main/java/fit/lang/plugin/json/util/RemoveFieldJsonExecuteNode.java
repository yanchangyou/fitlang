package fit.lang.plugin.json.util;

import com.alibaba.fastjson.JSONArray;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeBuildable;

import java.util.ArrayList;
import java.util.List;

/**
 * 执行节点
 */
public class RemoveFieldJsonExecuteNode extends JsonExecuteNode implements ExecuteNodeBuildable {

    List<String> fieldNames = new ArrayList<>();

    public void addRemoveField(String field) {
        fieldNames.add(field);
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        for (String field : input.getData().keySet()) {
            if (fieldNames.contains(field)) {
                continue;
            }
            output.set(field, input.get(field));
        }
    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {
        JSONArray array = ((JsonExecuteNodeData) executeNodeData).getData().getJSONArray("fieldNames");
        for (Object field : array) {
            fieldNames.add(field.toString());
        }
    }
}
