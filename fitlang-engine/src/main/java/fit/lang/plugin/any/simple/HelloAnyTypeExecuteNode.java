package fit.lang.plugin.any.simple;


import fit.lang.plugin.any.define.AnyTypeExecuteNode;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeData;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeInput;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeOutput;

/**
 * 执行节点
 */
public class HelloAnyTypeExecuteNode extends AnyTypeExecuteNode {

    @Override
    public void execute(AnyTypeExecuteNodeInput input, AnyTypeExecuteNodeOutput output) {
        String message = "hello, nobody!";
        if (input.getNodeData() != null) {
            Object who = input.getNodeData().getData();
            message = "hello, " + who + "!";
        }

        output.setNodeData(new AnyTypeExecuteNodeData(message));
    }
}
