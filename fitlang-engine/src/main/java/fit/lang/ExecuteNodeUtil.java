package fit.lang;

import cn.hutool.core.util.StrUtil;
import cn.hutool.system.SystemUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.ExecuteNode;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;

import java.awt.*;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.lang.reflect.Field;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.UUID;

/**
 * 工具类
 */
public class ExecuteNodeUtil {

    public static String DATE_FORMAT_TIMESTAMP = "yyyy-MM-dd HH:mm:ss.SSS";

    public static String DATE_FORMAT_DATETIME = "yyyy-MM-dd HH:mm:ss";

    /**
     * uuid
     *
     * @return
     */
    public static String uuid() {
        return UUID.randomUUID().toString();
    }

    /**
     * 获取时间戳，保留毫秒
     *
     * @return
     */
    public static String getTimestamp() {
        return getNow(DATE_FORMAT_TIMESTAMP);
    }

    /**
     * 获取时间戳，yyyy-MM-dd HH:mm:ss
     *
     * @return
     */
    public static String getNow() {
        return getNow(DATE_FORMAT_DATETIME);
    }

    /**
     * 获取当前时间，并且格式化
     *
     * @param format
     * @return
     */
    public static String getNow(String format) {
        return format(System.currentTimeMillis(), format);
    }

    /**
     * 日期对象格式化
     *
     * @param date
     * @param format
     * @return
     */
    public static String format(Date date, String format) {
        return new SimpleDateFormat(format).format(date);
    }

    /**
     * 对毫秒数格式化
     *
     * @param time
     * @param format
     * @return
     */
    public static String format(long time, String format) {
        return format(new Date(time), format);
    }

    /**
     * 对毫秒数格式化
     *
     * @param time
     * @return
     */
    public static String formatTimestamp(long time) {
        return format(new Date(time), DATE_FORMAT_TIMESTAMP);
    }

    public static void setExecuteNodeCommonAttribute(ExecuteNode executeNode, JSONObject nodeDefine) {
        String[] fields = new String[]{ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_UNI, ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_ID, ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_NAME};
        for (String field : fields) {
            setExecuteNodeCommonAttribute(executeNode, nodeDefine, field);
        }
        buildDefaultNodeId(executeNode);
    }

    public static void buildDefaultNodeId(ExecuteNode executeNode) {
        String nextId = executeNode.getNodeContext().buildNextNodeId(executeNode.getUni());
        if (StrUtil.isBlank(executeNode.getId())) {
            executeNode.setId(nextId);
            if (executeNode.getNodeDefine() != null && executeNode.getNodeDefine().getData() instanceof JSONObject) {
                ((JSONObject) executeNode.getNodeDefine().getData()).put(ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_ID, executeNode.getId());
            }
        }
    }

    public static void setExecuteNodeCommonAttribute(Object object, JSONObject jsonObject, String field) {

        if (jsonObject.getString(field) != null) {
            try {
                Field javaField = AbstractExecuteNode.class.getDeclaredField(field);
                javaField.setAccessible(true);
                javaField.set(object, jsonObject.getString(field));
            } catch (Exception e) {
                throw new ExecuteNodeException("setCommonAttribute error:", e);
            }
        }
    }

    public static void buildChildNode(ExecuteNode executeNode, JSONObject nodeDefine) {
        Object child = nodeDefine.get(ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_CHILD_NODE);
        if (child != null) {
            if (child instanceof JSONObject) {
                ExecuteNode childNode = JsonDynamicFlowExecuteEngine.createExecuteNode((JSONObject) child, executeNode.getNodeContext());
                executeNode.addChildNode(childNode);
            } else if (child instanceof JSONArray) {
                for (Object nextNode : ((JSONArray) child)) {
                    if (nextNode instanceof JSONObject) {
                        executeNode.addChildNode(JsonDynamicFlowExecuteEngine.createExecuteNode((JSONObject) nextNode, executeNode.getNodeContext()));
                    }
                }
            }
        }
    }

    public static void buildNextNode(ExecuteNode executeNode, JSONObject nodeDefine) {
        Object next = nodeDefine.get(ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_NEXT_NODE);
        if (next != null) {
            if (next instanceof JSONObject) {
                ExecuteNode nextNode = JsonDynamicFlowExecuteEngine.createExecuteNode((JSONObject) next, executeNode.getNodeContext());
                executeNode.addNextNode(nextNode);
            } else if (next instanceof JSONArray) {
                for (Object nextNode : ((JSONArray) next)) {
                    if (nextNode instanceof JSONObject) {
                        executeNode.addNextNode(JsonDynamicFlowExecuteEngine.createExecuteNode((JSONObject) nextNode, executeNode.getNodeContext()));
                    }
                }
            }
        }
    }

    public static String getExecuteNodeBasicInfo(ExecuteNode executeNode) {
        return executeNode.getUni() + "." + executeNode.getId();
    }

    /**
     * 获取所有的异常：包括深度遍历原因
     *
     * @param e
     * @return
     */
    public static String getAllException(Throwable e) {
        if (e == null) {
            return "";
        }
        String builder = e.getMessage() + "(" +
                getAllException(e.getCause()) + ")";
        return builder;
    }

    /**
     * 获取root异常：深度遍历原因
     *
     * @param e
     * @return
     */
    public static String getRootException(Throwable e) {
        if (e.getCause() != null) {
            return getRootException(e.getCause());
        }
        return e.getMessage();
    }

    /**
     * 获取user home 路径
     *
     * @return
     */
    public static String getUserHome() {
        String userHome = "";

        if (StrUtil.isBlank(userHome)) {
            userHome = SystemUtil.get("user.home");
        }

        if (StrUtil.isBlank(userHome)) {
            userHome = SystemUtil.get("HOME");
        }
        return userHome;
    }

    /**
     * 获取剪贴板内容
     *
     * @return
     */
    public static String getClipboard() {

        Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        Transferable contentTransferable = clipboard.getContents(null);
        if (contentTransferable.isDataFlavorSupported(DataFlavor.stringFlavor)) {
            try {
                return (String) contentTransferable.getTransferData(DataFlavor.stringFlavor);

            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        return "";
    }
}
