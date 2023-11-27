package fit.lang.plugin.json.cmd;

import cn.hutool.core.util.RuntimeUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.system.SystemUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * 执行节点
 */
public class CmdJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        List<String> cmdList = parseStringArray("cmd", input);

        if (cmdList == null || cmdList.isEmpty()) {
            throw new ExecuteNodeException("cmd is required!");
        }

        List<JSONObject> results = new ArrayList<>(cmdList.size());
        for (String cmd : cmdList) {
            JSONObject result = new JSONObject(2);
            cmd = parseCmd(cmd, input.getInputParamAndContextParam());
            result.put("cmd", cmd);
            String checkResult = checkCmd(cmd);
            List resultLines;
            if (checkResult != null) {
                resultLines = Collections.singletonList(checkResult);
            } else {
                cmd = cmd.trim();
                if (cmd.startsWith("#")) {
                    resultLines = Collections.singletonList("");
                } else {
                    cmd = wrapCmd(cmd);
                    resultLines = RuntimeUtil.execForLines(cmd);
                }
            }
            result.put("output", resultLines);
            results.add(result);
        }

        boolean isArray = isArrayField("cmd", input);

        output.set("result", isArray ? results : results.get(0));

    }

    /**
     * 解析命令行，支持参数
     *
     * @param cmd
     * @param inputParamAndContextParam
     * @return
     */
    String parseCmd(String cmd, JSONObject inputParamAndContextParam) {
        if (StrUtil.isBlank(cmd) || !cmd.contains("${")) {
            return cmd;
        }
        Matcher matcher = Pattern.compile("\\$\\{(\\w+)}").matcher(cmd);
        Set<String> param = new HashSet<>();
        while (matcher.find()) {
            param.add(matcher.group(1));
        }
        for (String key : param) {
            String value = inputParamAndContextParam.getString(key);
            value = value == null ? "" : value;
            cmd = cmd.replace("${".concat(key).concat("}"), value);
        }
        return cmd;
    }

    /**
     * 命令保障
     *
     * @param cmd
     * @return
     */
    private static String wrapCmd(String cmd) {
        if (!SystemUtil.getOsInfo().isWindows()) {
            if (cmd.contains("ping") && !cmd.contains("-c")) {
                cmd += " -c 4";
            }
        }
        return cmd;
    }

    /**
     * 检查高风险命令
     *
     * @param cmd
     * @return
     */
    String checkCmd(String cmd) {
        if (StrUtil.isBlank(cmd)) {
            return "cmd is empty!";
        }
        if (cmd.contains(">")
                || cmd.contains("^")
                || cmd.contains("&")
                || cmd.contains("!")
                || cmd.contains(";")
                || cmd.contains(")")
                || cmd.contains("]")
                || cmd.contains("}")
                || cmd.contains("*")
                || cmd.contains("|")
                || cmd.contains("@")
                || cmd.contains("$")
                || cmd.contains("~")
                || cmd.contains("`")
        ) {
            return "cmd is disabled";
        }
        if (cmd.matches("\\s*\\b(" +
                "|rm" +
                "|mv" +
                "|wget" +
                "|dd" +
                "|chmod" +
                "|sh" +
                "|shell" +
                "|bash" +
                "|zsh" +
                "|cp" +
                "|ulimit" +
                "|delete" +
                "|remove" +
                "|sleep" +
                "|kill" +
                "|ssh" +
                "|nohup" +
                "|su" +
                ")\\s+.+")) {
            return "cmd is disabled";
        }
        return null;
    }
}
