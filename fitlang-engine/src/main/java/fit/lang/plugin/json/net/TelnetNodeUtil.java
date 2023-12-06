package fit.lang.plugin.json.net;

import javax.net.ssl.SSLSocketFactory;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.Socket;

/**
 * 执行节点
 */
public class TelnetNodeUtil {

    static Socket buildSslSocket(String host, int port, Proxy proxy) throws IOException {
        SSLSocketFactory factory = (SSLSocketFactory) SSLSocketFactory.getDefault();
        if (proxy != null) {
            return factory.createSocket(new Socket(proxy), host, port, true);
        }
        return factory.createSocket(host, port);
    }

    static Socket buildSocket(String host, int port, Proxy proxy) throws IOException {
        if (proxy == null) {
            return new Socket(host, port);
        }
        Socket socket = new Socket(proxy);
        socket.connect(new InetSocketAddress(host, port));
        return socket;
    }
}
