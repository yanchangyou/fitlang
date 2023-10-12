package fit.lang.plugin.any.math;


import fit.lang.plugin.any.define.AnyTypeExecuteNode;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeData;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeInput;
import fit.lang.plugin.any.define.AnyTypeExecuteNodeOutput;

import java.util.List;

/**
 * 执行节点:加法
 */
public class AddIntAnyTypeExecuteNode extends AnyTypeExecuteNode {

    @Override
    public void execute(AnyTypeExecuteNodeInput input, AnyTypeExecuteNodeOutput output) {
        int sum = 0;
        if (input.getNodeData() != null) {
            List<Integer> list = (List<Integer>) input.getNodeData().getData();
            for (Integer num : list) {
                sum += num;
            }
        }

        output.setNodeData(new AnyTypeExecuteNodeData<>(sum));
    }
}
