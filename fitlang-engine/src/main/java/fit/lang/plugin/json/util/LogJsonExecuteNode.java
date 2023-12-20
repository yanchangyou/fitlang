package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.ExecuteNodeUtil.getTimestamp;

/**
 * 执行节点
 */
public class LogJsonExecuteNode extends JsonExecuteNode {

    static ExecuteNodeLogActionable printable;

    public static ExecuteNodeLogActionable getPrintable() {
        return printable;
    }

    public static void setPrintable(ExecuteNodeLogActionable printable) {
        LogJsonExecuteNode.printable = printable;
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        if (printable != null) {
            Object info = nodeJsonDefine.get("info");
            if (info != null) {
                Object infoObject = ExpressUtil.eval(info, input.getInputParamAndContextParam());
                if (infoObject instanceof JSONObject) {
                    ((JSONObject) infoObject).put("_timestamp", getTimestamp());
                }
                printable.print(infoObject);
            } else {
                printable.print(null);
            }
        }

        output.setData(input.getData());
    }
}