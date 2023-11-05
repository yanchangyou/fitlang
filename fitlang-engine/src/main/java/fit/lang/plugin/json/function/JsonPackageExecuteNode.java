package fit.lang.plugin.json.function;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.ExecuteNodeUtil;
import fit.lang.aop.ExecuteNodeSimpleAop;
import fit.lang.define.base.ExecuteContext;
import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.define.base.ExecuteNodeData;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.joinFilePath;

/**
 * 执行节点
 */
public class JsonPackageExecuteNode extends JsonExecuteNode implements ExecuteNodeBuildable {

    /**
     * 函数加载目录
     */
    static List<String> functionImportPaths = new ArrayList<>();

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

        //import 路径支持
        if (nodeDefineJson.getJSONArray("import") != null) {
            JSONArray paths = nodeDefineJson.getJSONArray("import");
            for (Object item : paths) {
                addImportPath(item.toString());
            }
        }
        ExecuteNodeUtil.buildChildNode(this, nodeDefineJson);
        for (ExecuteNode child : childNodes) {
            if (child.getName().equals(mainFunctionName)) {
                mainExecuteNode = (JsonExecuteNode) child;
            }
            String functionName = child.getName();
//            if (functionMap.containsKey(functionName)) {
//                throw new ExecuteNodeException("function name existed: ".concat(functionName));
//            }
            functionMap.put(functionName, (JsonExecuteNode) child);

            //排除main方法
            if (!"main".equals(functionName)) {
                String functionId = packageName.concat(".").concat(functionName);
//                if (packageFunctionMap.containsKey(functionId)) {
//                    throw new ExecuteNodeException("function existed: ".concat(functionId));
//                }
                packageFunctionMap.put(functionId, (JsonExecuteNode) child);
            }
        }
        importFunction(functionImportPaths, nodeJsonDefine, nodeContext);
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

    /**
     * 导入函数
     *
     * @param importRoots
     * @param nodeDefine
     * @param nodeContext
     */
    private static void importFunction(List<String> importRoots, JSONObject nodeDefine, ExecuteContext nodeContext) {
        JSONArray imports = nodeDefine.getJSONArray("import");
        if (imports != null) {
            for (Object importPath : imports) {
                for (String importRoot : importRoots) {
                    String path = importPath.toString();
                    if (!path.endsWith(".fit") && !path.endsWith(".fit.json")) {
                        path += ".fit";
                    }
                    File file = new File(joinFilePath(importRoot, path));
                    if (!file.exists()) {
                        continue;
                    }

                    String content = FileUtil.readUtf8String(file);
                    JSONObject define = JSONObject.parse(content);
                    JsonDynamicFlowExecuteEngine.createExecuteNode(define, nodeContext);
                    importFunction(importRoots, define, nodeContext);
                }
            }
        }
    }

    public static void addImportPath(String path) {
        functionImportPaths.add(path);
    }
}
