package fit.lang.plugin.json.net;

import java.io.IOException;
import java.net.Proxy;
import java.net.Socket;

/**
 * 执行节点
 */
public class SslTelnetHttpJsonExecuteNode extends TelnetHttpJsonExecuteNode {

    protected Socket buildSocket(String host, int port, Proxy proxy) throws IOException {
        return TelnetNodeUtil.buildSslSocket(host, port, proxy);
    }
}
