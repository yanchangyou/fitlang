package fit.lang.plugin.json.function;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.ExecuteNodeUtil;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.HashMap;
import java.util.Map;


/**
 * 执行节点
 */
public class JsonPackageExecuteNode extends JsonExecuteNode implements ExecuteNodeBuildable {

    String packageName;

    String mainFunctionName = "main";

    JsonExecuteNode mainExecuteNode;

    Map<String, JsonExecuteNode> functionMap = new HashMap<>();

    /**
     * 静态解析
     *
     * @param executeNodeData
     */
    @Override
    public void build(ExecuteNodeData executeNodeData) {

        JSONObject nodeDefineJson = (JSONObject) nodeDefine.getData();
        packageName = nodeDefineJson.getString("name");
        if (StrUtil.isBlank(packageName)) {
            throw new ExecuteNodeException("package name field is required!");
        }
        if (nodeDefineJson.getString("mainFunction") != null) {
            mainFunctionName = nodeDefineJson.getString("mainFunction");
        }
        ExecuteNodeUtil.buildChildNode(this, nodeDefineJson);
        for (ExecuteNode child : childNodes) {
            if (child.getName().equals(mainFunctionName)) {
                mainExecuteNode = (JsonExecuteNode) child;
            }
            functionMap.put(child.getName(), (JsonExecuteNode) child);
        }
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        if (mainExecuteNode == null) {
            throw new ExecuteNodeException("package main function is null, can not execute!");
        }

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        mainExecuteNode.executeAndNext(input, output);

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }
}
