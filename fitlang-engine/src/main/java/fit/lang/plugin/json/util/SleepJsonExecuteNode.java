package fit.lang.plugin.json.util;

import fit.lang.define.ExecuteNodeBuildable;
import fit.lang.define.ExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class SleepJsonExecuteNode extends JsonExecuteNode implements ExecuteNodeBuildable {

    long millis = 1000;

    public long getMillis() {
        return millis;
    }

    public void setMillis(long millis) {
        this.millis = millis;
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String second = parseStringField("second", input);
        if (second != null) {
            setMillis((long) (Double.parseDouble(second) * 1000));
        }

        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }

        output.setNodeData(input.getNodeData());
    }

    @Override
    public void build(ExecuteNodeData executeNodeData) {
    }
}
