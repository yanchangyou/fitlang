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

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static fit.lang.ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_FOREACH_FIELD_NAME;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getConfigFields;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getJsonData;

/**
 * 执行节点
 */
public class JsonForeachExecuteNode extends ForeachExecuteNode implements ExecuteNodeBuildable {

    public JsonForeachExecuteNode() {

    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {

        JSONObject nodeDefine = getJsonData(executeNodeData);

        setForeachField(nodeDefine.getString(DEFINE_KEYWORDS_OF_FOREACH_FIELD_NAME));
        setParallelism(nodeDefine.getInteger("parallelism"));
        setPipe(nodeDefine.getBoolean("isPipe"));
        if (nodeDefine.getString("indexName") != null) {
            setIndexName(nodeDefine.getString("indexName"));
        }

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
        Object target = jsonInput.get(foreachField);
        if (target instanceof JSONArray) {
            JSONArray list = (JSONArray) jsonInput.get(foreachField);
            currentIndex++;
            return list != null && !list.isEmpty() && currentIndex < list.size();
        } else if (target instanceof JSONObject) {
            JSONObject jsonObject = (JSONObject) jsonInput.get(foreachField);
            currentIndex++;
            return jsonObject != null && !jsonObject.isEmpty() && currentIndex < jsonObject.size();
        }
        return false;
    }

    Map<JSONObject, JSONArray> cache = new HashMap<>();

    JSONArray buildKeyValueList(JSONObject jsonObject) {
        if (cache.keySet().contains(jsonObject)) {
            return cache.get(jsonObject);
        }
        JSONArray list = new JSONArray();
        for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
            JSONObject item = new JSONObject();
            item.put("key", entry.getKey());
            item.put("value", entry.getValue());
            list.add(item);
        }
        cache.put(jsonObject, list);
        return list;
    }

    @Override
    public ExecuteNodeInput getCurrentInput(ExecuteNodeInput input) {
        JsonExecuteNodeInput jsonInput = (JsonExecuteNodeInput) input;
        Object target = jsonInput.get(foreachField);

        if (target == null) {
            throw new ExecuteNodeException("foreach ".concat(foreachField).concat(" value is null!"));
        }
        JSONArray list;
        if (target instanceof JSONObject) {
            list = buildKeyValueList((JSONObject) target);
        } else {
            list = jsonInput.getJsonArray(foreachField);
        }

        if (currentIndex > list.size()) {
            throw new ExecuteNodeException("JsonForeachExecuteNode out of bounder: " + currentIndex);
        }
        Object item = list.get(currentIndex);
        JSONObject itemJson;
        if (item instanceof Map) {
            if (item instanceof JSONObject) {
                itemJson = (JSONObject) item;
            } else {
                itemJson = JSONObject.from(item);
            }
        } else {
            itemJson = new JSONObject();
            itemJson.put("value", item);
        }
        JSONArray fields = getConfigFields(getJsonData(getNodeDefine()), "mixToItemField");
        for (Object fieldName : fields) {
            itemJson.put(fieldName.toString(), ((JsonExecuteNodeInput) input).get(fieldName.toString()));
        }
        return new JsonExecuteNodeInput(new JsonExecuteNodeData(itemJson), ((JsonExecuteNodeInput) input).getNodeContext());
    }

    @Override
    public ExecuteNodeOutput getCurrentOutput(ExecuteNodeOutput output) {
        return new JsonExecuteNodeOutput(new JsonExecuteNodeData(new JSONObject()), (JsonExecuteContext) output.getNodeContext());
    }

}
