package fit.lang.plugin.json.monitor;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.List;

/**
 * 获取监控信息
 */
public class GetMonitorDataJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        List<JSONObject> cpuGatherList = StartMonitorJsonExecuteNode.getCpuGatherList();
        JSONObject result = new JSONObject();
        result.put("cpu", cpuGatherList);

        output.setData(result);
    }

}
