package fit.lang.define.base;

/**
 * 执行上下文
 */
public interface ExecuteContext {

    String getInstanceId();

    Object getAttribute(String name);

    void setAttribute(String name, Object value);

    Object getAllAttribute();

    boolean isDebugMode();

    Object getNode(String nodeId);

    void addNode(String nodeId, Object nodeDefine);
}
