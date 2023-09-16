package fit.lang.plugin.json.util;

import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.define.base.ExecuteNodeData;

/**
 * 执行节点
 */
public class SleepJsonExecuteNode extends JsonExecuteNode implements ExecuteNodeBuildable {

    long millis;

    public long getMillis() {
        return millis;
    }

    public void setMillis(long millis) {
        this.millis = millis;
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String second = parseStringField("second", input);
        setMillis((long) (Double.parseDouble(second) * 1000));

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
