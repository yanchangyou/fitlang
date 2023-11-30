package fit.lang.plugin.json.net;

import cn.hutool.http.ssl.DefaultSSLFactory;

import java.io.IOException;
import java.net.Proxy;
import java.net.Socket;

/**
 * 执行节点
 */
public class SslTelnetHttpJsonExecuteNode extends TelnetHttpJsonExecuteNode {

    protected Socket buildSocket(Proxy proxy) throws IOException {
        return DefaultSSLFactory.getDefault().createSocket();
    }
}
