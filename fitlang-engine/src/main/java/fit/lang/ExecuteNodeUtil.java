package fit.lang;

import cn.hutool.core.util.StrUtil;
import cn.hutool.system.SystemUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.common.AbstractExecuteNode;
import fit.lang.define.base.ExecuteNode;

import java.lang.reflect.Field;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.UUID;

/**
 * 工具类
 */
public class ExecuteNodeUtil {

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
        return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(new Date());
    }

    /**
     * 获取时间戳，yyyy-MM-dd HH:mm:ss
     *
     * @return
     */
    public static String getNow() {
        return getNow("yyyy-MM-dd HH:mm:ss");
    }

    public static String getNow(String format) {
        return new SimpleDateFormat(format).format(new Date());
    }

    public static void setExecuteNodeCommonAttribute(ExecuteNode executeNode, JSONObject nodeDefine) {
        String[] fields = new String[]{ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_UNI, ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_ID, ExecuteNodeEngineConst.DEFINE_KEYWORDS_OF_NAME};
        for (String field : fields) {
            setExecuteNodeCommonAttribute(executeNode, nodeDefine, field);
        }
        buildDefaultNodeId(executeNode);
    }

    public static void buildDefaultNodeId(ExecuteNode executeNode) {
        if (executeNode.getId() == null) {
            executeNode.setId(uuid());
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
        StringBuilder builder = new StringBuilder();
        builder.append(e.getMessage()).append("(");
        builder.append(getAllException(e.getCause())).append(")");
        return builder.toString();
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

}
