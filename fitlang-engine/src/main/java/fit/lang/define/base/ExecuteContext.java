package fit.lang.define.base;

import fit.lang.info.NodeExecuteInfo;

/**
 * 执行上下文
 */
public interface ExecuteContext {

    String getInstanceId();

    Object getAttribute(String name);

    void setAttribute(String name, Object value);

    Object getAllAttribute();

    NodeExecuteInfo getNodeExecuteInfo(String nodeId);

    void setNodeExecuteInfo(String nodeId, NodeExecuteInfo nodeExecuteInfo);

    boolean isDebugMode();

    Object getNode(String nodeId);

    void addNode(String nodeId, Object nodeDefine);
}
