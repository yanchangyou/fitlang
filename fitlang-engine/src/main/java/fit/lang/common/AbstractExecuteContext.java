package fit.lang.common;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.base.ExecuteContext;
import fit.lang.info.NodeExecuteInfo;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 执行上下文
 */
public abstract class AbstractExecuteContext implements ExecuteContext {

    int nodeIndex = 100;

    Map<String, Object> attributeMap = new ConcurrentHashMap<>();

    Map<String, Map<String, Object>> nodeInputOutputMap = new ConcurrentHashMap<>();

    {
        attributeMap.put("nodeInputOutput", nodeInputOutputMap);
    }

    Map<String, NodeExecuteInfo> nodeExecuteInfoMap = new ConcurrentHashMap<>();

    Map<String, Object> nodeMap = new ConcurrentHashMap<>();

    Boolean debugMode;

    String instanceId = "I-" + System.currentTimeMillis();

    @Override
    public String buildNextNodeId() {
        return instanceId + "-" + nodeIndex++;
    }

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

    public void putAllAttribute(JSONObject jsonObject) {
        for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
            if (entry.getKey() != null && entry.getValue() != null) {
                attributeMap.put(entry.getKey(), entry.getValue());
            }
        }
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

    public void storeNodeInput(String nodeId, Object input) {
        storeNodeValue(nodeId, "input", input);
    }

    public void storeNodeOutput(String nodeId, Object output) {
        storeNodeValue(nodeId, "output", output);
    }

    public void storeNodeValue(String nodeId, String type, Object value) {
        Map<String, Object> nodeMap = nodeInputOutputMap.get(nodeId);
        if (nodeMap == null) {
            nodeMap = new LinkedHashMap<>();
            nodeInputOutputMap.put(nodeId, nodeMap);
        }
        nodeMap.put(type, value);
    }

}
