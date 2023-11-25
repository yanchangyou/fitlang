package fit.lang.plugin.json.cmd;

import cn.hutool.core.util.RuntimeUtil;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.ArrayList;
import java.util.List;


/**
 * 执行节点
 */
public class CmdJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        List<String> cmdList = parseStringArray("cmd", input);

        if (cmdList == null || cmdList.isEmpty()) {
            throw new ExecuteNodeException("cmd is required!");
        }

        List<List<String>> results = new ArrayList<>(cmdList.size());
        for (String cmd : cmdList) {
            List<String> result = RuntimeUtil.execForLines(cmd);
            results.add(result);
        }

        boolean isArray = isArrayField("cmd", input);

        output.set("result", isArray ? results : results.get(0));

    }
}
