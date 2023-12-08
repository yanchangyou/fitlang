package fit.lang.plugin.json.net;

import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.Socket;

/**
 * 工具类
 */
public class TelnetNodeUtil {

    static Socket buildSocket(String host, int port, Proxy proxy) throws IOException {
        if (proxy == null) {
            return new Socket(host, port);
        }
        Socket socket = new Socket(proxy);
        socket.connect(new InetSocketAddress(host, port));
        return socket;
    }

    static Socket buildSslSocket(String host, int port, Proxy proxy) throws IOException {
        SSLSocketFactory factory = (SSLSocketFactory) SSLSocketFactory.getDefault();
        SSLSocket socket;
        if (proxy != null) {
            Socket proxySocket = new Socket();
            proxySocket.connect(proxy.address());
            socket = (SSLSocket) factory.createSocket(proxySocket, host, port, true);
        } else {
            socket = (SSLSocket) factory.createSocket(host, port);
        }
        socket.startHandshake();
        return socket;
    }

}
