package fit.lang.plugin.json.monitor;

import cn.hutool.core.util.NumberUtil;
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

        String secondText = parseStringField("second", input);

        int second = 600;
        if (secondText != null && NumberUtil.isInteger(secondText)) {
            second = Integer.parseInt(secondText);
        }

        List<JSONObject> cpuGatherList = StartMonitorJsonExecuteNode.getCpuGatherList(second);
        JSONObject result = new JSONObject();
        result.put("cpu", cpuGatherList);
        result.put("second", second);

        output.setData(result);
    }

}
