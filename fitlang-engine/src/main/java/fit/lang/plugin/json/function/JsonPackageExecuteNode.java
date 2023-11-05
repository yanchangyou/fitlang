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

    /**
     * 按照惯例，默认从main函数开始执行
     */
    String mainFunctionName = "main";

    JsonExecuteNode mainExecuteNode;

    Map<String, JsonExecuteNode> functionMap = new HashMap<>();

    static Map<String, JsonExecuteNode> localFunctionMap;

    /**
     * 全局函数定义
     */
    static Map<String, JsonExecuteNode> packageFunctionMap = new HashMap<>();

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
            String functionName = child.getName();
            if (functionMap.containsKey(functionName)) {
                throw new ExecuteNodeException("function name existed: ".concat(functionName));
            }
            functionMap.put(functionName, (JsonExecuteNode) child);

            String functionId = packageName.concat(".").concat(functionName);
            if (packageFunctionMap.containsKey(functionId)) {
                throw new ExecuteNodeException("function existed: ".concat(functionId));
            }
            packageFunctionMap.put(functionId, (JsonExecuteNode) child);
        }
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        if (mainExecuteNode == null) {
            throw new ExecuteNodeException("package main function node is null, can not execute! main function name is: ".concat(mainFunctionName).concat("."));
        }

        //局部函数赋值
        localFunctionMap = functionMap;

        ExecuteNodeSimpleAop.beforeExecute(input, this, output);

        mainExecuteNode.executeAndNext(input, output);

        ExecuteNodeSimpleAop.afterExecute(input, this, output);

    }

    public static JsonExecuteNode getFunction(String functionName) {

        //先找包内找
        JsonExecuteNode node = localFunctionMap.get(functionName);

        //全局找
        if (node == null) {
            node = packageFunctionMap.get(functionName);
        }
        return node;
    }
}
