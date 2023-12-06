package fit.lang.plugin.json.net;

import cn.hutool.core.io.IoUtil;
import cn.hutool.core.io.LineHandler;
import cn.hutool.core.util.StrUtil;
import cn.hutool.http.HttpUtil;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.IOException;
import java.io.OutputStream;
import java.net.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.buildProxy;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.getUrlPort;

/**
 * 执行节点
 */
public class TelnetHttpJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String url = ExecuteJsonNodeUtil.parseStringField("url", input, nodeJsonDefine);
        if (url == null) {
            throw new ExecuteNodeException("telnetHttp node url field is required!");
        }

        String method = ExecuteJsonNodeUtil.parseStringField("method", input, nodeJsonDefine);

        JSONObject header = nodeJsonDefine.getJSONObject("header");
        header = ExpressUtil.eval(header, input.getInputParamAndContextParam());

        try {

            URL httpUrl = new URL(url);

            String urlHost = httpUrl.getHost();
            int port = getUrlPort(httpUrl);
            String path = httpUrl.getPath();
            if (StrUtil.isBlank(path)) {
                path = "/";
            }
            if (StrUtil.isBlank(method)) {
                method = "GET";
            }
            List<String> inputLines = new ArrayList<>();
            inputLines.add(method.concat(" ").concat(path).concat(" HTTP/1.0"));
            if (header == null) {
                header = new JSONObject();
            }
            if (!header.containsKey("host") && !header.containsKey("Host") && !header.containsKey("HOST")) {
                header.put("Host", urlHost);
            }
            for (Map.Entry<String, Object> entry : header.entrySet()) {
                inputLines.add(entry.getKey().concat(": ").concat(StrUtil.toString(entry.getValue())));
            }
            inputLines.add("");

            Proxy proxy = buildProxy(nodeJsonDefine.getJSONObject("proxy"));
            Socket socket = buildSocket(urlHost, port, proxy);
            OutputStream outputStream = socket.getOutputStream();
            for (String line : inputLines) {
                outputStream.write((line + "\n").getBytes());
                outputStream.flush();
            }
            List<String> outputLines = new ArrayList<>();
            IoUtil.readUtf8Lines(socket.getInputStream(), new LineHandler() {
                @Override
                public void handle(String line) {
                    outputLines.add(line);
                }
            });
            socket.close();
            output.set("url", url);
            output.set("urlHost", urlHost);
            output.set("host", header.get("Host"));
            output.set("port", port);
            output.set("inputHeader", header);
//            output.set("input", JSON.toJSON(inputLines));
            if (outputLines.size() > 0) {
                String statusLine = outputLines.get(0);
                String[] parts = statusLine.split(" ");
                if (parts.length >= 3) {
                    output.set("http", parts[0]);
                    output.set("status", Integer.parseInt(parts[1]));
                    String message = statusLine.substring(statusLine.indexOf(" ", parts[0].length() + 2) + 1);
                    output.set("message", message);
                }
                JSONObject outputHeader = new JSONObject();
                int headerAndBodySeparatorIndex = 1;
                //header 解
                for (int i = 1; i < outputLines.size(); i++) {
                    String line = outputLines.get(i);
                    if (StrUtil.isBlank(line)) {
                        headerAndBodySeparatorIndex = i;
                        break;
                    }
                    String[] headerKeyValue = line.split(": ");
                    outputHeader.put(headerKeyValue[0], line.substring(headerKeyValue[0].length() + 2));
                }
                output.set("outputHeader", outputHeader);

                StringBuilder builder = new StringBuilder();
                for (int i = headerAndBodySeparatorIndex + 1; i < outputLines.size(); i++) {
                    if (i != headerAndBodySeparatorIndex + 1) {
                        builder.append("\n");
                    }
                    builder.append(outputLines.get(i));
                }
                String body = builder.toString();
                output.set("body", body);
            }
            output.set("output", JSON.toJSON(outputLines));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    protected Socket buildSocket(String host, int port, Proxy proxy) throws IOException {
        if (proxy == null) {
            return new Socket(host, port);
        }
        Socket socket = new Socket(proxy);
        socket.connect(new InetSocketAddress(host, port));
        return socket;
    }
}
