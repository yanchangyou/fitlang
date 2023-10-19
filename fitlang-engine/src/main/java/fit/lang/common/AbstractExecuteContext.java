package fit.lang.common;

import fit.lang.define.base.ExecuteContext;
import fit.lang.info.NodeExecuteInfo;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 执行上下文
 */
public abstract class AbstractExecuteContext implements ExecuteContext {

    Map<String, Object> attributeMap = new ConcurrentHashMap<>();

    Map<String, NodeExecuteInfo> nodeExecuteInfoMap = new ConcurrentHashMap<>();

    Map<String, Object> nodeMap = new ConcurrentHashMap<>();

    Boolean debugMode;

    String instanceId;

    public void setInstanceId(String instanceId) {
        this.instanceId = instanceId;
    }

    @Override
    public boolean isDebugMode() {
        return debugMode != null && debugMode;
    }

    public void setDebugMode(Boolean debugMode) {
        this.debugMode = debugMode;
    }

    public String getInstanceId() {
        return instanceId;
    }

    @Override
    public Object getAttribute(String name) {
        return attributeMap.get(name);
    }

    @Override
    public void setAttribute(String name, Object value) {
        attributeMap.put(name, value);
    }

    public Map<String, Object> getAllAttribute() {
        return attributeMap;
    }

    @Override
    public Object getNode(String nodeId) {
        return nodeMap.get(nodeId);
    }

    @Override
    public void addNode(String nodeId, Object nodeDefine) {
        nodeMap.put(nodeId, nodeDefine);
    }

    public NodeExecuteInfo getNodeExecuteInfo(String nodeId) {
        return nodeExecuteInfoMap.get(nodeId);
    }

    public void setNodeExecuteInfo(String nodeId, NodeExecuteInfo nodeExecuteInfo) {
        nodeExecuteInfoMap.put(nodeId, nodeExecuteInfo);
    }

}
