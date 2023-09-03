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
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import fit.lang.plugin.json.flow.*;
import fit.lang.plugin.json.info.SystemBaseInfoJsonExecuteNode;
import fit.lang.plugin.json.tool.FileServerJsonExecuteNode;
import fit.lang.plugin.json.tool.HttpJsonExecuteNode;
import fit.lang.plugin.json.tool.ProxyJsonExecuteNode;
import fit.lang.plugin.json.tool.ServerJsonExecuteNode;
import fit.lang.plugin.json.util.*;
import fit.lang.plugin.json.web.WebJsonExecuteNode;
import fit.lang.plugin.json.web.WebSocketServerJsonExecuteNode;

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
            throw new ExecuteNodeException("flow define is empty!");
        }
        if (nodeDefine.get(ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_UNI) == null) {
            throw new ExecuteNodeException("node uni is empty!");
        }
        this.nodeDefine = nodeDefine;
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

        register("mix", MixJsonExecuteNode.class);
        register("replace", ReplaceJsonExecuteNode.class);
        register("eval", EvalJsonExecuteNode.class);
        register("set", SetJsonExecuteNode.class);

        register("node", JsonNodeExecuteNode.class);

        register("sleep", SleepJsonExecuteNode.class);

        // tool
        register("http", HttpJsonExecuteNode.class);
        register("server", ServerJsonExecuteNode.class);
        register("fileServer", FileServerJsonExecuteNode.class);
        register("proxy", ProxyJsonExecuteNode.class);

        // web
        register("web", WebJsonExecuteNode.class);
        register("websocketServer", WebSocketServerJsonExecuteNode.class);

        //info
        register("systemInfo", SystemBaseInfoJsonExecuteNode.class);


    }
}
