package fit.lang.define;

import fit.lang.info.NodeExecuteInfo;

import java.util.List;

/**
 * 执行节点：参考servlet的请求响应模式
 */
public interface ExecuteNode {

    /**
     * 上下文
     *
     * @return
     */
    ExecuteContext getNodeContext();

    NodeExecuteInfo getNodeExecuteInfo();

    void setNodeContext(ExecuteContext nodeContext);

    /**
     * 统一节点标识符
     *
     * @return
     */
    String getUni();

    void setUni(String id);

    String getId();

    void setId(String id);

    String getName();

    void setName(String name);

    String getDescription();

    void setDescription(String name);

    /**
     * 是否需要保持入参不变
     *
     * @return
     */
    boolean isNeedCloneInputData();

    void setNeedCloneInputData(Boolean isNeedCloneInputData);

    String getNextMode();

    void setNextMode(String nextMode);

    void setNodeDefine(ExecuteNodeData nodeDefine);

    ExecuteNodeData getNodeDefine();

    List<ExecuteNode> getNextNodes();

    void addNextNode(ExecuteNode nextNode);

    void addNextNodes(List<ExecuteNode> nodes);

    List<ExecuteNode> getChildNodes();

    void addChildNode(ExecuteNode childNode);

    void addChildNodes(List<ExecuteNode> nodes);

    /**
     * 执行节点
     *
     * @param input
     * @param output
     */
    void execute(ExecuteNodeInput input, ExecuteNodeOutput output);

    void executeAndNext(ExecuteNodeInput input, ExecuteNodeOutput output);

}
