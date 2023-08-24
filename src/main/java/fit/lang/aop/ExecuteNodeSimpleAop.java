package fit.lang.aop;

import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeAopIgnoreTag;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;

/**
 * aop简单实现，支持前后添加代码，处理公共切面逻辑
 */
public class ExecuteNodeSimpleAop {

    public static void beforeExecute(ExecuteNodeInput input, ExecuteNode executeNode, ExecuteNodeOutput output) {
        if (!(executeNode instanceof ExecuteNodeAopIgnoreTag)) {
            String id = executeNode.getId();
            if (id == null) {
                id = executeNode.toString();
            }
            Object data = input.getNodeData().getData();
            if (executeNode.isNeedCloneInputData()) {
                input.getNodeData().setData(input.getNodeData().cloneData());
            }
            input.getNodeContext().setAttribute(id + "Input", data);
        }
    }

    public static void afterExecute(ExecuteNodeInput input, ExecuteNode executeNode, ExecuteNodeOutput output) {
        if (!(executeNode instanceof ExecuteNodeAopIgnoreTag)) {
            String id = executeNode.getId();
            if (id == null) {
                id = executeNode.toString();
            }
            input.getNodeContext().setAttribute(id + "Output", output.getNodeData().getData());
        }
    }
}