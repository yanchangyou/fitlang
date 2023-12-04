package fit.lang.plugin.json.cmd;

import cn.hutool.core.io.IoUtil;
import cn.hutool.core.util.RuntimeUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.system.SystemUtil;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getSystemCharset;

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

        String[] envArray = new String[0];
        JSONObject env = nodeJsonDefine.getJSONObject("env");
        if (env != null) {
            env = ExpressUtil.eval(env, input.getInputParamAndContextParam());
            envArray = new String[env.size()];
            int index = 0;
            for (Map.Entry<String, Object> entry : env.entrySet()) {
                if (entry.getValue() == null) {
                    continue;
                }
                envArray[index++] = entry.getKey().concat("=").concat(entry.getValue().toString());
            }
        }

        JSONObject option = nodeJsonDefine.getJSONObject("option");
        if (option != null) {
            option = ExpressUtil.eval(option, input.getInputParamAndContextParam());
        }

        Object target = nodeJsonDefine.get("target");
        if (target != null) {
            target = ExpressUtil.eval(target, input.getInputParamAndContextParam());
        }

        Object param = nodeJsonDefine.get("param");
        if (param != null) {
            param = ExpressUtil.eval(param, input.getInputParamAndContextParam());
        }

        List<JSONObject> results = new ArrayList<>(cmdList.size());
        for (String cmd : cmdList) {
            JSONObject result = new JSONObject(2);
            cmd = parseCmd(cmd, input.getInputParamAndContextParam());
            String checkResult = checkCmd(cmd);
            List resultLines;
            if (checkResult != null) {
                resultLines = Collections.singletonList(checkResult);
            } else {
                cmd = cmd.trim();
                if (cmd.startsWith("#")) {
                    resultLines = Collections.singletonList("");
                } else {
                    cmd = wrapCmd(cmd, option, target, param);
                    try {
                        Process process;

                        if (JsonDynamicFlowExecuteEngine.getCurrentDir() != null) {
                            process = RuntimeUtil.exec(envArray, new File(JsonDynamicFlowExecuteEngine.getCurrentDir()), cmd);
                        } else {
                            process = RuntimeUtil.exec(envArray, cmd);
                        }

                        resultLines = IoUtil.readUtf8Lines(process.getErrorStream(), new ArrayList<>());
                        if (resultLines == null || resultLines.isEmpty()) {
                            resultLines = IoUtil.readLines(process.getInputStream(), getSystemCharset(), new ArrayList<>());
                        }
                    } catch (Throwable e) {
                        resultLines = Collections.singletonList(e.getMessage());
                    }
                }
            }
            result.put("cmd", cmd);
            if (env != null && !env.isEmpty()) {
                result.put("env", env);
            }
            result.put("out", resultLines);
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
     * @param option
     * @param target
     * @param param
     * @return
     */
    private static String wrapCmd(String cmd, JSONObject option, Object target, Object param) {

        cmd = parseOption(cmd, option);
        cmd = parseTarget(cmd, target);
        cmd = parseTarget(cmd, param);

        if (!SystemUtil.getOsInfo().isWindows()) {
            if (cmd.contains("ping") && !cmd.contains("-c")) {
                cmd += " -c 4";
            }
        }
        return cmd.replaceAll(" +", " ").trim();
    }

    private static String parseTarget(String cmd, Object target) {
        if (target == null || "".equals(target)) {
            return cmd;
        }
        String targetString;
        if (target instanceof String) {
            targetString = (String) target;
        } else if (target instanceof List) {
            List list = (List) target;
            StringBuilder builder = new StringBuilder();
            for (Object item : list) {
                if (builder.length() > 0) {
                    builder.append(" ");
                }
                builder.append(item);
            }
            targetString = builder.toString();
        } else if (target instanceof JSONObject) {
            targetString = parseOption("", (JSONObject) target);
        } else {
            throw new ExecuteNodeException("only support string, array, object type");
        }
        return cmd.concat(" ").concat(targetString);
    }

    private static String parseOption(String cmd, JSONObject param) {
        if (param == null || param.isEmpty()) {
            return cmd;
        }
        StringBuilder builder = new StringBuilder();
        for (Map.Entry<String, Object> entry : param.entrySet()) {
            if (builder.length() > 0) {
                builder.append(" ");
            }
            String key = entry.getKey();
            if (StrUtil.isBlank(key)) {
                continue;
            }
            builder.append(key);
            if (entry.getValue() != null && !"".equals(entry.getValue())) {
                if (!key.endsWith(":") && !key.endsWith("=")) {
                    builder.append(" ");
                }
                builder.append(entry.getValue());
            }
        }
        return cmd.concat(" ").concat(builder.toString());
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
//                || cmd.contains(";")
                || cmd.contains(")")
                || cmd.contains("]")
//                || cmd.contains("}")
                || cmd.contains("*")
                || cmd.contains("|")
                || cmd.contains("@")
//                || cmd.contains("$")
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
//                "|sh" +
//                "|shell" +
//                "|bash" +
//                "|zsh" +
                "|cp" +
                "|ulimit" +
                "|delete" +
                "|remove" +
                "|sleep" +
                "|kill" +
                "|ssh" +
                "|nohup" +
                "|su" +
                "|vim" +
                "|vi" +
                "|nona" +
                ")\\s+.+")) {
            return "cmd is disabled";
        }
        return null;
    }
}
