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
import fit.lang.define.base.ExecuteContext;
import fit.lang.define.base.ExecuteNode;
import fit.lang.define.base.ExecuteNodeAopIgnoreTag;
import fit.lang.define.base.ExecuteNodeBuildable;
import fit.lang.plugin.json.cloud.CloudClientJsonExecuteNode;
import fit.lang.plugin.json.cloud.CloudGetClientJsonExecuteNode;
import fit.lang.plugin.json.cloud.CloudServerJsonExecuteNode;
import fit.lang.plugin.json.define.*;

import fit.lang.plugin.json.excel.ReadExcelJsonExecuteNode;
import fit.lang.plugin.json.excel.WriteExcelJsonExecuteNode;
import fit.lang.plugin.json.file.DeleteFileJsonExecuteNode;
import fit.lang.plugin.json.file.ReadFileJsonExecuteNode;
import fit.lang.plugin.json.file.WriteFileJsonExecuteNode;
import fit.lang.plugin.json.flow.*;
import fit.lang.plugin.json.git.GitPullJsonExecuteNode;
import fit.lang.plugin.json.http.*;
import fit.lang.plugin.json.info.SystemBaseInfoJsonExecuteNode;
import fit.lang.plugin.json.json.*;
import fit.lang.plugin.json.monitor.*;
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

        ExecuteNode executeNode = createExecuteNode(nodeDefine, input.getNodeContext());

        executeNode.executeAndNext(input, output);

    }

    public static List<ExecuteNode> createExecuteNode(JSONArray childNodes, ExecuteContext nodeContext) {
        List<ExecuteNode> childExecuteNodes = new ArrayList<>(childNodes.size());
        for (Object childNode : childNodes) {
            JSONObject node = (JSONObject) childNode;
            ExecuteNode childExecuteNode = JsonDynamicFlowExecuteEngine.createExecuteNode(node, nodeContext);
            childExecuteNodes.add(childExecuteNode);
        }
        return childExecuteNodes;
    }

    public static ExecuteNode createExecuteNode(JSONObject nodeDefine, ExecuteContext nodeContext) {

        String uni = nodeDefine.getString(ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_UNI);
        if (uni == null) {
            throw new ExecuteNodeException("node uni is required!");
        }
        ExecuteNode executeNode = createExecuteNode(uni, nodeDefine, nodeContext);

        return executeNode;
    }

    public static ExecuteNode createExecuteNode(String uni, JSONObject nodeDefine, ExecuteContext nodeContext) {
        Class<? extends ExecuteNode> executeNodeClass = getExecuteNodeClass(uni);
        if (executeNodeClass == null) {
            throw new ExecuteNodeException("not register uni: " + uni);
        }
        try {
            ExecuteNode executeNode = JSON.to(executeNodeClass, nodeDefine);
            executeNode.setNodeDefine(new JsonExecuteNodeData(nodeDefine));
            executeNode.setNodeContext(nodeContext);
            ExecuteNodeUtil.setExecuteNodeCommonAttribute(executeNode, nodeDefine);
            nodeContext.addNode(executeNode.getId(), nodeDefine);

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
            return;
        }
        executeNodeMap.put(uni, executeNodeClass);
    }

    public static void unregister(String uni) {
        executeNodeMap.remove(uni);
    }

    public static void disableUnsafeNodes() {
        String[] dangerNodes = new String[]{
                "readFile",
                "writeFile",
                "deleteFile",
                "readExcel",
                "writeExcel"
        };
        for (String nodeUni : dangerNodes) {
            unregister(nodeUni);
        }
    }

    public static void enableUnsafeNodes() {

        //excel
        register("readExcel", ReadExcelJsonExecuteNode.class);
        register("writeExcel", WriteExcelJsonExecuteNode.class);

        // file
        register("readFile", ReadFileJsonExecuteNode.class);
        register("writeFile", WriteFileJsonExecuteNode.class);
        register("deleteFile", DeleteFileJsonExecuteNode.class);

    }

    static {

        // util
        register("hello", HelloJsonExecuteNode.class);
        register("echo", EchoExecuteNode.class);
        register("print", PrintExecuteNode.class);
        register("sleep", SleepJsonExecuteNode.class);
        register("assert", AssertJsonExecuteNode.class);
        register("timeCount", JsonTimeCountExecuteNode.class);

        // flow
        register("pipe", JsonPipeExecuteNode.class);
        register("sequence", JsonSequenceExecuteNode.class);
        register("foreach", JsonForeachExecuteNode.class);
        register("loop", JsonLoopExecuteNode.class);
        register("switch", JsonSwitchExecuteNode.class);
        register("return", ReturnJsonExecuteNode.class);
        register("thread", ThreadExecuteNode.class);
        register("call", CallJsonExecuteNode.class);

        register("execute", ExecuteJsonExecuteNode.class);

        //json
        register("parseJson", ParseJsonJsonExecuteNode.class);
        register("stringifyJson", StringifyJsonJsonExecuteNode.class);

        register("mix", MixJsonExecuteNode.class);
        register("eval", EvalJsonExecuteNode.class);
        register("set", SetJsonExecuteNode.class);

        register("mixNode", MixNodeJsonExecuteNode.class);

        register("node", JsonNodeExecuteNode.class);

        register("removeField", RemoveFieldJsonExecuteNode.class);
        register("removeEmptyField", RemoveEmptyFieldJsonExecuteNode.class);

        register("convert", ConvertJsonExecuteNode.class);
        register("convertKeyValueList", ConvertKeyValueListJsonExecuteNode.class);

        register("add", AddJsonExecuteNode.class);

        register("increase", IncreaseJsonExecuteNode.class);
        register("decrease", DecreaseJsonExecuteNode.class);

        // http
        register("http", HttpJsonExecuteNode.class);
        register("postJson", HttpPostJsonJsonExecuteNode.class);
        register("httpPostJson", HttpPostJsonJsonExecuteNode.class);
        register("postForm", HttpPostFormJsonExecuteNode.class);
        register("httpPostForm", HttpPostFormJsonExecuteNode.class);
        register("httpGet", HttpGetJsonExecuteNode.class);
        register("httpPut", HttpPutJsonJsonExecuteNode.class);
        register("httpDelete", HttpDeleteJsonJsonExecuteNode.class);

        // web
        register("server", ServerJsonExecuteNode.class);
        register("proxy", ProxyJsonExecuteNode.class);
        register("register", CloudServerJsonExecuteNode.class);

        register("web", WebJsonExecuteNode.class);
        register("wsServer", WebSocketServerJsonExecuteNode.class);
        register("wsClient", WebSocketClientJsonExecuteNode.class);

        // cloud
        register("cloudServer", CloudServerJsonExecuteNode.class);
        register("cloudClient", CloudClientJsonExecuteNode.class);
        register("cloudGetClient", CloudGetClientJsonExecuteNode.class);

        //info
        register("systemInfo", SystemBaseInfoJsonExecuteNode.class);

        //monitor
        register("startMonitor", StartMonitorJsonExecuteNode.class);
        register("getMonitorData", GetMonitorDataJsonExecuteNode.class);
        register("getClientMonitorData", GetClientMonitorDataJsonExecuteNode.class);
        register("receiveClientMonitorData", ReceiveClientMonitorDataJsonExecuteNode.class);
        register("pushClientMonitorData", PushClientMonitorDataJsonExecuteNode.class);
        register("getMonitorClient", GetMonitorClientJsonExecuteNode.class);

        //git
        register("gitPull", GitPullJsonExecuteNode.class);

        //excel
        register("readExcel", ReadExcelJsonExecuteNode.class);
        register("writeExcel", WriteExcelJsonExecuteNode.class);

        // file
        register("readFile", ReadFileJsonExecuteNode.class);
        register("writeFile", WriteFileJsonExecuteNode.class);
        register("deleteFile", DeleteFileJsonExecuteNode.class);

    }
}
