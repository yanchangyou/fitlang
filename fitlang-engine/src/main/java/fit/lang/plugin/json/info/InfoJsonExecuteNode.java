package fit.lang.plugin.json.info;

import cn.hutool.system.SystemUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 获取系统信息：依赖hutool实现
 */
public class InfoJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        JSONArray shardFields = nodeJsonDefine.getJSONArray("shareFields");
        if (shardFields == null) {
            return;
        }

        JSONObject systemInfoJson = new JSONObject();

        if (shardFields.contains("os")) {
            systemInfoJson.put("os", SystemUtil.getOsInfo());
        }

        if (shardFields.contains("host")) {
            systemInfoJson.put("host", SystemUtil.getHostInfo());
        }

        if (shardFields.contains("memory")) {
            JSONObject memory = new JSONObject();
            memory.put("total", SystemUtil.getTotalMemory());
            memory.put("free", SystemUtil.getFreeMemory());
            memory.put("max", SystemUtil.getMaxMemory());
            systemInfoJson.put("memory", memory);
        }

        if (shardFields.contains("runtime")) {
            systemInfoJson.put("runtime", SystemUtil.getJavaRuntimeInfo());
        }

        if (shardFields.contains("javaSpec")) {
            systemInfoJson.put("javaSpec", SystemUtil.getJavaSpecInfo());
        }

        if (shardFields.contains("jvm")) {
            systemInfoJson.put("jvm", SystemUtil.getJvmInfo());
        }

        if (shardFields.contains("user")) {
            systemInfoJson.put("user", SystemUtil.getUserInfo());
        }

        if (shardFields.contains("properties")) {
            systemInfoJson.put("properties", SystemUtil.getProps());
        }

        output.setData(systemInfoJson);

    }
}