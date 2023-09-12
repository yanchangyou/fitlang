package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeEngineConst;
import fit.lang.ExecuteNodeException;
import fit.lang.ExecuteNodeUtil;
import fit.lang.common.flow.ThreadExecuteNode;
import fit.lang.common.util.EchoExecuteNode;
import fit.lang.common.util.PrintExecuteNode;
import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeAopIgnoreTag;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.plugin.json.cloud.CloudClientJsonExecuteNode;
import fit.lang.plugin.json.cloud.CloudServerJsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import fit.lang.plugin.json.excel.ReadExcelJsonExecuteNode;
import fit.lang.plugin.json.excel.WriteExcelJsonExecuteNode;
import fit.lang.plugin.json.flow.*;
import fit.lang.plugin.json.info.SystemBaseInfoJsonExecuteNode;
import fit.lang.plugin.json.web.HttpPostJsonJsonExecuteNode;
import fit.lang.plugin.json.web.ProxyJsonExecuteNode;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;
import fit.lang.plugin.json.util.*;
import fit.lang.plugin.json.web.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static fit.lang.ExecuteNodeUtil.buildNextNode;

/**
 * 执行引擎
 */
public class JsonDynamicFlowExecuteEngine extends JsonExecuteNode implements ExecuteNodeAopIgnoreTag {

    static Map<String, Class<? extends ExecuteNode>> executeNodeMap = new HashMap<>();

    JSONObject nodeDefine;

    public JsonDynamicFlowExecuteEngine(JSONObject nodeDefine) {
        setNodeDefine(nodeDefine);
    }

    public void setNodeDefine(JSONObject nodeDefine) {
        if (nodeDefine == null || nodeDefine.isEmpty()) {
            throw new ExecuteNodeException("define is empty!");
        }
        if (nodeDefine.get(ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_UNI) == null) {
            throw new ExecuteNodeException("node uni is empty!");
        }
        //避免修改内部结构
        this.nodeDefine = nodeDefine.clone();
    }

    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        ExecuteNode executeNode = createExecuteNode(nodeDefine);

        executeNode.executeAndNext(input, output);

    }

    public static List<ExecuteNode> createExecuteNode(JSONArray childNodes) {
        List<ExecuteNode> childExecuteNodes = new ArrayList<>(childNodes.size());
        for (Object childNode : childNodes) {
            JSONObject node = (JSONObject) childNode;
            ExecuteNode childExecuteNode = JsonDynamicFlowExecuteEngine.createExecuteNode(node);
            childExecuteNodes.add(childExecuteNode);
        }
        return childExecuteNodes;
    }

    public static ExecuteNode createExecuteNode(JSONObject nodeDefine) {

        String uni = nodeDefine.getString(ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_UNI);
        if (uni == null) {
            throw new ExecuteNodeException("node uni is required!");
        }
        ExecuteNode executeNode = createExecuteNode(uni, nodeDefine);

        return executeNode;
    }

    public static ExecuteNode createExecuteNode(String uni, JSONObject nodeDefine) {
        Class<? extends ExecuteNode> executeNodeClass = getExecuteNodeClass(uni);
        if (executeNodeClass == null) {
            throw new ExecuteNodeException("not register uni: " + uni);
        }
        try {
            ExecuteNode executeNode = JSON.to(executeNodeClass, nodeDefine);

            ExecuteNodeUtil.setExecuteNodeCommonAttribute(executeNode, nodeDefine);

            executeNode.setNodeDefine(new JsonExecuteNodeData(nodeDefine));

            buildNextNode(executeNode, nodeDefine);

            if (executeNode instanceof ExecuteNodeBuildable) {
                ((ExecuteNodeBuildable) executeNode).build(new JsonExecuteNodeData(nodeDefine));
            }

            return executeNode;
        } catch (Exception e) {
            throw new ExecuteNodeException("buildExecuteNode exception:", e);
        }
    }

    /**
     * 获取执行类：支持协议解析
     *
     * @param uni
     * @return
     */
    public static Class<? extends ExecuteNode> getExecuteNodeClass(String uni) {
        String key = uni;
        if (uni.contains(":")) {
            key = uni.substring(0, uni.lastIndexOf(':'));
        }
        return executeNodeMap.get(key);
    }

    public static void register(String uni, Class<? extends ExecuteNode> executeNodeClass) {
        if (executeNodeMap.containsKey(uni)) {
            throw new ExecuteNodeException("uni is existed:" + uni);
        }
        executeNodeMap.put(uni, executeNodeClass);
    }

    static {

        // util
        register("hello", HelloJsonExecuteNode.class);
        register("echo", EchoExecuteNode.class);

        register("print", PrintExecuteNode.class);
        register("add", AddJsonExecuteNode.class);

        register("removeField", RemoveFieldJsonExecuteNode.class);
        register("removeEmptyField", RemoveEmptyFieldJsonExecuteNode.class);

        register("convert", ConvertJsonExecuteNode.class);

        // flow
        register("pipe", JsonPipeExecuteNode.class);
        register("sequence", JsonSequenceExecuteNode.class);
        register("foreach", JsonForeachExecuteNode.class);
        register("loop", JsonLoopExecuteNode.class);
        register("switch", JsonSwitchExecuteNode.class);
        register("return", ReturnJsonExecuteNode.class);
        register("thread", ThreadExecuteNode.class);

        register("assert", AssertJsonExecuteNode.class);

        register("mix", MixJsonExecuteNode.class);
        register("replace", ReplaceJsonExecuteNode.class);
        register("eval", EvalJsonExecuteNode.class);
        register("set", SetJsonExecuteNode.class);

        register("mixNode", MixNodeJsonExecuteNode.class);

        register("node", JsonNodeExecuteNode.class);

        register("execute", ExecuteJsonExecuteNode.class);

        register("sleep", SleepJsonExecuteNode.class);

        // web
        register("http", HttpPostJsonJsonExecuteNode.class);
        register("postJson", HttpPostJsonJsonExecuteNode.class);
        register("postForm", HttpPostFormJsonExecuteNode.class);
        register("get", HttpGetJsonExecuteNode.class);
        register("httpPut", HttpPutJsonJsonExecuteNode.class);
        register("httpDelete", HttpDeleteJsonJsonExecuteNode.class);

        register("server", ServerJsonExecuteNode.class);
        register("proxy", ProxyJsonExecuteNode.class);
        register("register", CloudServerJsonExecuteNode.class);

        register("web", WebJsonExecuteNode.class);
        register("wsServer", WebSocketServerJsonExecuteNode.class);
        register("wsClient", WebSocketClientJsonExecuteNode.class);

        // cloud
        register("cloudServer", CloudServerJsonExecuteNode.class);
        register("cloudClient", CloudClientJsonExecuteNode.class);

        //info
        register("systemInfo", SystemBaseInfoJsonExecuteNode.class);

        //excel
        register("readExcel", ReadExcelJsonExecuteNode.class);
        register("writeExcel", WriteExcelJsonExecuteNode.class);

    }
}
