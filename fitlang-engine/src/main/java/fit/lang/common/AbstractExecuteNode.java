package fit.lang.common;

import fit.lang.define.base.*;

import java.util.ArrayList;
import java.util.List;

/**
 * 执行节点
 */
public abstract class AbstractExecuteNode implements ExecuteNode {

    protected ExecuteContext nodeContext;

    public ExecuteContext getNodeContext() {
        return nodeContext;
    }

    public void setNodeContext(ExecuteContext nodeContext) {
        this.nodeContext = nodeContext;
    }

    /**
     * 统一节点标识符
     */
    protected String uni;

    /**
     * 节点id
     */
    protected String id;

    /**
     * 节点名称
     */
    protected String name;

    /**
     * 节点描述
     */
    protected String description;

    /**
     * 是否需要保持入参数据不变
     */
    protected Boolean needCloneInputData;

    /**
     * 是否next的执行模式是否pipe
     */
    protected String nextMode;

    protected ExecuteNodeData nodeDefine;

    protected List<ExecuteNode> nextNodes = new ArrayList<>(1);

    protected List<ExecuteNode> childNodes = new ArrayList<>(1);

    @Override
    public String getUni() {
        return uni;
    }

    @Override
    public void setUni(String uni) {
        this.uni = uni;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String getDescription() {
        return description;
    }

    @Override
    public void setDescription(String description) {
        this.description = description;
    }

    public boolean isNeedCloneInputData() {
        return needCloneInputData != null && needCloneInputData;
    }

    public void setNeedCloneInputData(Boolean needCloneInputData) {
        this.needCloneInputData = needCloneInputData;
    }

    public String getNextMode() {
        return nextMode;
    }

    public void setNextMode(String nextMode) {
        this.nextMode = nextMode;
    }

    boolean isPipeNext() {
        return "pipe".equals(nextMode);
    }

    @Override
    public ExecuteNodeData getNodeDefine() {
        return nodeDefine;
    }

    public void setNodeDefine(ExecuteNodeData nodeDefine) {
        this.nodeDefine = nodeDefine;
    }

    @Override
    public List<ExecuteNode> getNextNodes() {
        return nextNodes;
    }

    @Override
    public void addNextNode(ExecuteNode nextNode) {
        nextNodes.add(nextNode);
    }

    @Override
    public void addNextNodes(List<ExecuteNode> nodes) {
        for (ExecuteNode node : nodes) {
            nextNodes.add(node);
        }
    }

    @Override
    public List<ExecuteNode> getChildNodes() {
        return childNodes;
    }

    @Override
    public void addChildNode(ExecuteNode childNode) {
        childNodes.add(childNode);
    }

    public void addChildNodes(List<ExecuteNode> nodes) {
        for (ExecuteNode node : nodes) {
            childNodes.add(node);
        }
    }

    public void executeAndNext(ExecuteNodeInput input, ExecuteNodeOutput output) {

        execute(input, output);

        //next是否执行采用pipe模式
        if (isPipeNext()) {
            input.setNodeData(output.getNodeData().cloneThis());
        }

        //顺序执行next节点
        if (getNextNodes() != null) {
            for (ExecuteNode nextNode : getNextNodes()) {
                if (this.isNeedCloneInputData()) {
                    input.getNodeData().setData(input.getNodeData().cloneData());
                }
                nextNode.executeAndNext(input, output);
            }
        }
    }
}
