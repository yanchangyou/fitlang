package fit.lang.plugin.json.net;

import javax.net.ssl.SSLSocketFactory;
import java.io.IOException;
import java.net.Proxy;
import java.net.Socket;

/**
 * 执行节点
 */
public class SslTelnetJsonExecuteNode extends TelnetJsonExecuteNode {

    protected Socket buildSocket(String host, int port, Proxy proxy) throws IOException {
        SSLSocketFactory factory = (SSLSocketFactory) SSLSocketFactory.getDefault();
        if (proxy != null) {
            return factory.createSocket(new Socket(proxy), host, port, true);
        }
        return factory.createSocket(host, port);
    }
}
