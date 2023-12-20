package fit.lang.plugin.json.util;

import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

/**
 * 执行节点
 */
public class PrintJsonExecuteNode extends JsonExecuteNode {

    static ExecuteNodePrintable printable;

    public static ExecuteNodePrintable getPrintable() {
        return printable;
    }

    public static void setPrintable(ExecuteNodePrintable printable) {
        PrintJsonExecuteNode.printable = printable;
    }

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        if (printable != null) {
            Object info = nodeJsonDefine.get("info");
            if (info != null) {
                Object infoObject = ExpressUtil.eval(info, input.getInputParamAndContextParam());
                printable.print(infoObject);
            } else {
                printable.print(null);
            }
        }

        output.setData(input.getData());
    }
}