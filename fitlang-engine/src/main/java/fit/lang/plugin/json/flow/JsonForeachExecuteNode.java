package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.ExecuteNodeException;
import fit.lang.common.flow.ForeachExecuteNode;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.plugin.json.define.JsonExecuteContext;

import java.util.List;

import static fit.lang.ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_FOREACH_FIELD_NAME;

/**
 * 执行节点
 */
public class JsonForeachExecuteNode extends ForeachExecuteNode implements ExecuteNodeBuildable {

    public JsonForeachExecuteNode() {

    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {

        JSONObject nodeDefine = ((JsonExecuteNodeData) executeNodeData).getData();

        setForeachField(nodeDefine.getString(DEFINE_KEYWORDS_OF_FOREACH_FIELD_NAME));

        ExecuteNodeUtil.buildChildNode(this, nodeDefine);
    }

    String foreachField = "list";

    int currentIndex = -1;

    public String getForeachField() {
        return foreachField;
    }

    public void setForeachField(String foreachField) {
        this.foreachField = foreachField;
    }

    public int getCurrentIndex() {
        return currentIndex;
    }

    public void setCurrentIndex(int currentIndex) {
        this.currentIndex = currentIndex;
    }

    @Override
    public void setForeachOutputList(List<ExecuteNodeData> foreachOutputList, ExecuteNodeOutput output) {

        JSONArray array = new JSONArray(foreachOutputList.size());
        for (ExecuteNodeData data : foreachOutputList) {
            array.add(data == null ? null : data.getData());
        }
        ((JsonExecuteNodeOutput) output).getData().put(foreachField, array);

        //重置index，下次使用
        currentIndex = -1;
    }

    @Override
    public boolean next(ExecuteNodeInput input) {
        JsonExecuteNodeInput jsonInput = (JsonExecuteNodeInput) input;
        JSONArray list = jsonInput.getJsonArray(foreachField);
        currentIndex++;
        return list != null && !list.isEmpty() && currentIndex < list.size();
    }

    @Override
    public ExecuteNodeInput getCurrentInput(ExecuteNodeInput input) {
        JsonExecuteNodeInput jsonInput = (JsonExecuteNodeInput) input;
        JSONArray list = jsonInput.getJsonArray(foreachField);
        if (currentIndex > list.size()) {
            throw new ExecuteNodeException("JsonForeachExecuteNode out of bounder: " + currentIndex);
        }
        Object item = list.get(currentIndex);
        JSONObject itemJson;
        if (item instanceof JSONObject) {
            itemJson = (JSONObject) item;
        } else {
            itemJson = new JSONObject();
            itemJson.put("data", item);
        }
        return new JsonExecuteNodeInput(new JsonExecuteNodeData(itemJson), ((JsonExecuteNodeInput) input).getNodeContext());
    }

    @Override
    public ExecuteNodeOutput getCurrentOutput(ExecuteNodeOutput output) {
        return new JsonExecuteNodeOutput(new JsonExecuteNodeData(new JSONObject()), (JsonExecuteContext) output.getNodeContext());
    }

}
