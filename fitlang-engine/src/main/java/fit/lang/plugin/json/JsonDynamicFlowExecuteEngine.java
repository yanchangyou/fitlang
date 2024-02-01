package fit.lang.plugin.json;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeEngineConst;
import fit.lang.ExecuteNodeException;
import fit.lang.ExecuteNodeUtil;
import fit.lang.ExecuteReturnNodeException;
import fit.lang.common.flow.ThreadExecuteNode;
import fit.lang.common.util.EchoExecuteNode;
import fit.lang.common.util.PrintExecuteNode;
import fit.lang.define.ExecuteContext;
import fit.lang.define.ExecuteNode;
import fit.lang.define.ExecuteNodeAopIgnoreTag;
import fit.lang.define.ExecuteNodeBuildable;
import fit.lang.plugin.json.cmd.CmdJsonExecuteNode;
import fit.lang.plugin.json.cmd.UnzipJsonExecuteNode;
import fit.lang.plugin.json.cmd.ZipJsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeData;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import fit.lang.plugin.json.file.DeleteFileJsonExecuteNode;
import fit.lang.plugin.json.file.ReadFileJsonExecuteNode;
import fit.lang.plugin.json.file.WriteFileJsonExecuteNode;
import fit.lang.plugin.json.flow.*;
import fit.lang.plugin.json.function.JsonFunctionExecuteNode;
import fit.lang.plugin.json.function.JsonPackageExecuteNode;
import fit.lang.plugin.json.http.*;
import fit.lang.plugin.json.ide.*;
import fit.lang.plugin.json.ide.message.*;
import fit.lang.plugin.json.info.InfoJsonExecuteNode;
import fit.lang.plugin.json.info.SystemBaseInfoJsonExecuteNode;
import fit.lang.plugin.json.json.*;
import fit.lang.plugin.json.monitor.*;
import fit.lang.plugin.json.net.SslTelnetHttpJsonExecuteNode;
import fit.lang.plugin.json.net.SslTelnetJsonExecuteNode;
import fit.lang.plugin.json.net.TelnetHttpJsonExecuteNode;
import fit.lang.plugin.json.net.TelnetJsonExecuteNode;
import fit.lang.plugin.json.office.MergeExcelJsonExecuteNode;
import fit.lang.plugin.json.office.ReadExcelForAllSheetJsonExecuteNode;
import fit.lang.plugin.json.office.ReadExcelJsonExecuteNode;
import fit.lang.plugin.json.office.WriteExcelJsonExecuteNode;
import fit.lang.plugin.json.os.GetClipboardJsonExecuteNode;
import fit.lang.plugin.json.os.SetClipboardJsonExecuteNode;
import fit.lang.plugin.json.util.*;
import fit.lang.plugin.json.web.ProxyJsonExecuteNode;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;
import fit.lang.plugin.json.web.WebJsonExecuteNode;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static fit.lang.ExecuteNodeUtil.buildNextNode;
import static fit.lang.ExecuteNodeUtil.getAllException;

/**
 * 执行引擎
 */
public class JsonDynamicFlowExecuteEngine extends JsonExecuteNode implements ExecuteNodeAopIgnoreTag {

    public static String currentDir;

    static Map<String, Class<? extends ExecuteNode>> executeNodeMap = new HashMap<>();

    JSONObject nodeDefine;

    public static String getCurrentDir() {
        return currentDir;
    }

    public static void setCurrentDir(String currentDir) {
        JsonDynamicFlowExecuteEngine.currentDir = currentDir;
    }

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

        if (StrUtil.isNotBlank(currentDir)) {
            input.getNodeContext().setAttribute("currentDir", currentDir);
        }
        try {
            ExecuteNode executeNode = createExecuteNode(nodeDefine, input.getNodeContext());
            executeNode.executeAndNext(input, output);
        } catch (ExecuteReturnNodeException returnNodeException) {
            output.setData(returnNodeException.getResult());
        }

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

            if (uni.contains(":")) {
                nodeDefine.put(uni.substring(0, uni.indexOf(':')), uni.substring(uni.indexOf(':') + 1));
            }

            return executeNode;
        } catch (Exception e) {
            throw new ExecuteNodeException("buildExecuteNode exception:".concat(getAllException(e)), e);
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
        };
        for (String nodeUni : dangerNodes) {
            unregister(nodeUni);
        }
    }

    public static void enableUnsafeNodes() {

        // file
        register("readFile", ReadFileJsonExecuteNode.class);
        register("writeFile", WriteFileJsonExecuteNode.class);
        register("deleteFile", DeleteFileJsonExecuteNode.class);

    }

    static {

        // util
        register("hello", HelloJsonExecuteNode.class);
        register("echo", EchoExecuteNode.class);
        register("setGlobal", SetGlobalJsonExecuteNode.class);
        register("print", PrintExecuteNode.class);
        register("log", LogJsonExecuteNode.class);
        register("sleep", SleepJsonExecuteNode.class);
        register("perf", PerformanceJsonExecuteNode.class);
        register("replaceContent", ReplaceContentJsonExecuteNode.class);

        // flow
        register("sequence", JsonSequenceExecuteNode.class);
        register("batch", JsonBatchExecuteNode.class);
        register("pipe", JsonPipeExecuteNode.class);
        register("foreach", JsonForeachExecuteNode.class);
        register("loop", JsonLoopExecuteNode.class);
        register("switch", JsonSwitchExecuteNode.class);
        register("thread", ThreadExecuteNode.class);
        register("call", CallJsonExecuteNode.class);
        register("catch", CatchJsonExecuteNode.class);
        register("assert", AssertJsonExecuteNode.class);
        register("return", ReturnJsonExecuteNode.class);
        register("execute", ExecuteJsonExecuteNode.class);

        //json
        register("parseJson", ParseJsonJsonExecuteNode.class);
        register("stringifyJson", StringifyJsonJsonExecuteNode.class);
        register("parse", ParseJsonJsonExecuteNode.class);
        register("stringify", StringifyJsonJsonExecuteNode.class);

        register("mix", MixJsonExecuteNode.class);
        register("eval", EvalJsonExecuteNode.class);
        register("mixNode", MixNodeJsonExecuteNode.class);

        register("getSchema", GetSchemaJsonExecuteNode.class);
        register("get", GetJsonExecuteNode.class);
        register("set", SetJsonExecuteNode.class);

        register("removeField", RemoveFieldJsonExecuteNode.class);
        register("removeEmptyField", RemoveEmptyFieldJsonExecuteNode.class);

        register("sortField", SortFieldJsonExecuteNode.class);
        register("getStruct", GetStructJsonExecuteNode.class);

        register("convert", ConvertJsonExecuteNode.class);
        register("convertKeyValueList", ConvertKeyValueListJsonExecuteNode.class);
        register("convertArrayToObject", ConvertArrayToObjectJsonExecuteNode.class);
        register("convertObjectToArray", ConvertObjectToArrayJsonExecuteNode.class);
        register("convertToObjectArray", ConvertToObjectArrayJsonExecuteNode.class);
        register("convertToBasicArray", ConvertToBasicArrayJsonExecuteNode.class);

        register("add", AddJsonExecuteNode.class);

        register("increase", IncreaseJsonExecuteNode.class);
        register("decrease", DecreaseJsonExecuteNode.class);

        // http
        register("http", HttpJsonExecuteNode.class);

        register("httpHead", HttpHeadJsonExecuteNode.class);
        register("HEAD http", HttpHeadJsonExecuteNode.class);
        register("HEAD https", HttpHeadJsonExecuteNode.class);

        register("postJson", HttpPostJsonJsonExecuteNode.class);
        register("httpPostJson", HttpPostJsonJsonExecuteNode.class);
        register("POST http", HttpPostJsonJsonExecuteNode.class);
        register("POST https", HttpPostJsonJsonExecuteNode.class);

        register("postForm", HttpPostFormJsonExecuteNode.class);
        register("httpPostForm", HttpPostFormJsonExecuteNode.class);

        register("httpGet", HttpGetJsonExecuteNode.class);
        register("GET http", HttpGetJsonExecuteNode.class);
        register("GET https", HttpGetJsonExecuteNode.class);

        register("httpPut", HttpPutJsonJsonExecuteNode.class);
        register("PUT http", HttpPutJsonJsonExecuteNode.class);
        register("PUT https", HttpPutJsonJsonExecuteNode.class);

        register("httpDelete", HttpDeleteJsonJsonExecuteNode.class);
        register("DELETE http", HttpDeleteJsonJsonExecuteNode.class);
        register("DELETE https", HttpDeleteJsonJsonExecuteNode.class);

        // web
        register("server", ServerJsonExecuteNode.class);
        register("proxy", ProxyJsonExecuteNode.class);

        register("web", WebJsonExecuteNode.class);

        //info
        register("systemInfo", SystemBaseInfoJsonExecuteNode.class);
        register("info", InfoJsonExecuteNode.class);

        //monitor
        register("startMonitor", StartMonitorJsonExecuteNode.class);
        register("getMonitorData", GetMonitorDataJsonExecuteNode.class);
        register("getClientMonitorData", GetClientMonitorDataJsonExecuteNode.class);
        register("receiveClientMonitorData", ReceiveClientMonitorDataJsonExecuteNode.class);
        register("pushClientMonitorData", PushClientMonitorDataJsonExecuteNode.class);
        register("getMonitorClient", GetMonitorClientJsonExecuteNode.class);

        // file
        register("readFile", ReadFileJsonExecuteNode.class);
        register("writeFile", WriteFileJsonExecuteNode.class);
        register("deleteFile", DeleteFileJsonExecuteNode.class);

        //jui
        register("postman", HttpJsonExecuteNode.class);

        //function and package
        register("package", JsonPackageExecuteNode.class);
        register("function", JsonFunctionExecuteNode.class);

        //cmd
        register("cmd", CmdJsonExecuteNode.class);
        register("zip", ZipJsonExecuteNode.class);
        register("unzip", UnzipJsonExecuteNode.class);

        //net
        register("telnet", TelnetJsonExecuteNode.class);
        register("telnets", SslTelnetJsonExecuteNode.class);
        register("telnet.http", TelnetHttpJsonExecuteNode.class);
        register("telnet.https", SslTelnetHttpJsonExecuteNode.class);

        //ide
        register("readEditor", ReadEditorJsonExecuteNode.class);
        register("writeEditor", WriteEditorJsonExecuteNode.class);
        register("openWebPage", OpenWebPageJsonExecuteNode.class);
        register("showHtml", ShowHtmlJsonExecuteNode.class);
        register("showJsonPage", ShowJsonPageJsonExecuteNode.class);
        register("showConfig", ShowConfigJsonExecuteNode.class);
        register("readConfig", ReadConfigJsonExecuteNode.class);
        register("showGlobalConfigDialog", ShowGlobalConfigDialogJsonExecuteNode.class);
        register("chooseFile", ChooseFileJsonExecuteNode.class);
        register("showInfoMessage", ShowInfoMessageJsonExecuteNode.class);
        register("showWarningMessage", ShowWarningMessageJsonExecuteNode.class);
        register("showErrorMessage", ShowErrorMessageJsonExecuteNode.class);
        register("showInputDialog", ShowInputDialogJsonExecuteNode.class);
        register("showOkCancelDialog", ShowOkCancelDialogJsonExecuteNode.class);
        register("showYesNoCancelDialog", ShowYesNoCancelDialogJsonExecuteNode.class);
        register("showCheckboxOkCancelDialog", ShowCheckboxOkCancelDialogJsonExecuteNode.class);
        register("showPasswordDialog", ShowPasswordDialogJsonExecuteNode.class);
        register("showDiff", ShowDiffJsonExecuteNode.class);

        //os
        register("getClipboard", GetClipboardJsonExecuteNode.class);
        register("setClipboard", SetClipboardJsonExecuteNode.class);

        //office
        register("readExcelForAllSheet", ReadExcelForAllSheetJsonExecuteNode.class);
        register("readExcel", ReadExcelJsonExecuteNode.class);
        register("writeExcel", WriteExcelJsonExecuteNode.class);
        register("mergeExcel", MergeExcelJsonExecuteNode.class);

    }
}
