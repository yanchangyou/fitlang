package fit.lang.aop;

import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeAopIgnoreTag;
import fit.lang.define.base.ExecuteNodeInput;
import fit.lang.define.base.ExecuteNodeOutput;
import fit.lang.info.NodeExecuteInfo;

import static fit.lang.ExecuteNodeUtil.buildDefaultNodeId;

/**
 * aop简单实现，支持前后添加代码，处理公共切面逻辑
 */
public class ExecuteNodeSimpleAop {

    public static void beforeExecute(ExecuteNodeInput input, ExecuteNode executeNode, ExecuteNodeOutput output) {

        buildDefaultNodeId(executeNode);

        String id = executeNode.getId();
        if (!(executeNode instanceof ExecuteNodeAopIgnoreTag)) {
            Object data = input.getNodeData().getData();
            if (executeNode.isNeedCloneInputData()) {
                input.getNodeData().setData(input.getNodeData().cloneData());
            }
            input.getNodeContext().storeNodeInput(id, data);
        }

        if (executeNode.getNodeContext() != null) {
            if (executeNode.getNodeExecuteInfo() == null) {
                executeNode.getNodeContext().setNodeExecuteInfo(id, new NodeExecuteInfo());
            }
            executeNode.getNodeExecuteInfo().setBeginTime(System.currentTimeMillis());
//            executeNode.getNodeExecuteInfo().increaseBeginCount();
        }
    }

    public static void afterExecute(ExecuteNodeInput input, ExecuteNode executeNode, ExecuteNodeOutput output) {
        String id = executeNode.getId();

        if (!(executeNode instanceof ExecuteNodeAopIgnoreTag)) {
            input.getNodeContext().storeNodeOutput(id, output.getNodeData().getData());
        }
        if (executeNode.getNodeContext() != null) {
            executeNode.getNodeContext().getNodeExecuteInfo(id).setEndTime(System.currentTimeMillis());
//            executeNode.getNodeExecuteInfo().increaseEndCount();
        }
    }
}