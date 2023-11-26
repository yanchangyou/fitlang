package fit.lang.plugin.json.net;

import sun.security.ssl.SSLSocketFactoryImpl;

import java.io.IOException;
import java.net.Socket;

/**
 * 执行节点
 */
public class SslTelnetJsonExecuteNode extends TelnetJsonExecuteNode {

    protected Socket getSocket() throws IOException {
        return SSLSocketFactoryImpl.getDefault().createSocket();
    }
}
