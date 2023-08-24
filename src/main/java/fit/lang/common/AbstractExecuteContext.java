package fit.lang.common;

import fit.lang.define.base.ExecuteContext;

import java.util.Map;
import java.util.TreeMap;

/**
 * 执行上下文
 */
public abstract class AbstractExecuteContext implements ExecuteContext {

    Map<String, Object> attributeMap = new TreeMap<>();

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

}
