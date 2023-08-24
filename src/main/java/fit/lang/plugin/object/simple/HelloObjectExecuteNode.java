package fit.lang.plugin.object.simple;


import fit.lang.plugin.object.define.ObjectExecuteNode;
import fit.lang.plugin.object.define.ObjectExecuteNodeData;
import fit.lang.plugin.object.define.ObjectExecuteNodeOutput;
import fit.lang.plugin.object.define.ObjectExecuteNodeInput;

/**
 * 执行节点
 */
public class HelloObjectExecuteNode extends ObjectExecuteNode {

    @Override
    public void execute(ObjectExecuteNodeInput input, ObjectExecuteNodeOutput output) {
        String message = "hello, nobody!";
        if (input.getNodeData() != null) {
            Object who = input.getNodeData().getData();
            message = "hello, " + who + "!";
        }

        output.setNodeData(new ObjectExecuteNodeData(message));
    }
}
