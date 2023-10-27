package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeUtil;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.common.flow.LoopExecuteNode;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;

import java.util.List;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.parseStringField;

/**
 * 执行节点
 */
public class JsonLoopExecuteNode extends LoopExecuteNode implements ExecuteNodeBuildable {

    /**
     * 静态解析
     *
     * @param executeNodeData
     */
    @Override
    public void build(ExecuteNodeData executeNodeData) {

        JSONObject nodeDefineJson = (JSONObject) nodeDefine.getData();

        setPipe(Boolean.TRUE.equals(nodeDefineJson.getBoolean("isPipe")));
        setBagsMode(Boolean.TRUE.equals(nodeDefineJson.getBoolean("isBagsMode")));
        setBagsName(nodeDefineJson.getString("bagsName"));

        setParallelism(nodeDefineJson.getInteger("parallelism"));

        ExecuteNodeUtil.buildChildNode(this, nodeDefineJson);
    }

    @Override
    public void execute(ExecuteNodeInput input, ExecuteNodeOutput output) {

        //动态解析
        String loopTimes = parseStringField("loopTimes", (JsonExecuteNodeInput) input, (JSONObject) nodeDefine.getData());
        if (loopTimes != null) {
            setLoopTimes(Integer.parseInt(loopTimes));
        }

        super.execute(input, output);
    }

    @Override
    public List getBags(int size) {
        return new JSONArray(size);
    }

    @Override
    public void setBags(String bagsFieldName, List list, ExecuteNodeOutput output) {
        JSONObject data = new JSONObject();
        data.put(bagsFieldName, list);
        ((JsonExecuteNodeData) output.getNodeData()).setData(data);
    }
}
