package fit.lang.plugin.map.simple;

import fit.lang.plugin.map.define.MapExecuteNode;
import fit.lang.plugin.map.define.MapExecuteNodeInput;
import fit.lang.plugin.map.define.MapExecuteNodeOutput;

/**
 * 执行节点
 */
public class HelloMapExecuteNode extends MapExecuteNode {

    @Override
    public void execute(MapExecuteNodeInput input, MapExecuteNodeOutput output) {
        Object who = input.getNodeData().getData().get("who");
        String message = "hello, " + who + "!";

        output.getNodeData().getData().put("message", message);
    }
}
