package fit.lang.plugin.json.net;

import cn.hutool.core.io.IoUtil;
import cn.hutool.core.io.LineHandler;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;

/**
 * 执行节点
 */
public class TelnetJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String host = parseStringField("host", input);
        int port = parseIntField("port", input, -1);
        List<String> inputLines = parseStringArray("input", input);
        try {
            Socket socket = new Socket();
            socket.connect(new InetSocketAddress(host, port));
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
            output.set("output", outputLines);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
