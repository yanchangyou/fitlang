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
        String method = ExecuteJsonNodeUtil.parseStringField("method", input, nodeJsonDefine);

        JSONObject header = nodeJsonDefine.getJSONObject("header");
        header = ExpressUtil.eval(header, input.getInputParamAndContextParam());

        if (url == null) {
            throw new ExecuteNodeException("telnetHttp node url field is required!");
        }
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
            Socket socket = buildSocket(proxy);
            socket.connect(new InetSocketAddress(urlHost, port));
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
            output.set("input", JSON.toJSON(inputLines));
            output.set("output", JSON.toJSON(outputLines));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    protected Socket buildSocket(Proxy proxy) throws IOException {
        return proxy == null ? new Socket() : new Socket(proxy);
    }
}
