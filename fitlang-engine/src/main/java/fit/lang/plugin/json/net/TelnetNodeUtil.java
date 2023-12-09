package fit.lang.plugin.json.net;

import javax.net.ssl.*;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.Socket;

/**
 * 工具类
 */
public class TelnetNodeUtil {

    static {
        //调试日志
//        System.setProperty("javax.net.debug", "ssl:handshake");
    }

    static Socket buildSocket(String host, int port, Proxy proxy) throws IOException {
        if (proxy == null) {
            return new Socket(host, port);
        }
        Socket socket = new Socket(proxy);
        socket.connect(new InetSocketAddress(host, port));
        return socket;
    }

    static Socket buildSslSocket(String host, int port, Proxy proxy, boolean validateCert) throws IOException {
        SSLSocketFactory factory;
        if (validateCert) {
            factory = (SSLSocketFactory) SSLSocketFactory.getDefault();
        } else {
            factory = getUnsafeSslSocketFactory();
        }
        SSLSocket socket;
        if (proxy != null) {
            Socket proxySocket = new Socket();
            proxySocket.connect(proxy.address());
            if (Proxy.Type.HTTP.equals(proxy.type())) {
                doTunnelHandshake(proxySocket, host, port);
            }
            host = getHost(host);
            socket = (SSLSocket) factory.createSocket(proxySocket, host, port, true);
        } else {
            socket = (SSLSocket) factory.createSocket(host, port);
        }
        return socket;
    }

    static String getHost(String host) {
        return host.contains(" ") ? host.split(" +")[1] : host;
    }

    static String getIP(String host) {
        return host.contains(" ") ? host.split(" +")[0] : host;
    }

    /*
    https://docs.oracle.com/en/java/javase/11/security/sample-code-illustrating-secure-socket-connection-client-and-server.html#GUID-A4D59ABB-62AF-4FC0-900E-A795FDC84E41

    //打开握手调试日志
    https://www.baeldung.com/java-ssl-handshake-failures

     * Tell our tunnel where we want to CONNECT, and look for the
     * right reply.  Throw IOException if anything goes wrong.
     */
    private static void doTunnelHandshake(Socket tunnel, String host, int port) throws IOException {
        String ip = getIP(host);
        host = getHost(host);
        OutputStream out = tunnel.getOutputStream();
        String msg = "CONNECT " + ip + ":" + port + " HTTP/1.0\n" //connect
                + "Host: " + host + "\n"// host
                + "\n";//换行符只能有一个，不能多
        byte b[];
        try {
            /*
             * We really do want ASCII7 -- the http protocol doesn't change
             * with locale.
             */
            b = msg.getBytes("ASCII7");
        } catch (UnsupportedEncodingException ignored) {
            /*
             * If ASCII7 isn't there, something serious is wrong, but
             * Paranoia Is Good (tm)
             */
            b = msg.getBytes();
        }
        out.write(b);
        out.flush();

        /*
         * We need to store the reply so we can create a detailed
         * error message to the user.
         */
        byte reply[] = new byte[200];
        int replyLen = 0;
        int newlinesSeen = 0;
        boolean headerDone = false;     /* Done on first newline */

        InputStream in = tunnel.getInputStream();
        boolean error = false;

        while (newlinesSeen < 2) {
            int i = in.read();
            if (i < 0) {
                throw new IOException("Unexpected EOF from proxy");
            }
            if (i == '\n') {
                headerDone = true;
                ++newlinesSeen;
            } else if (i != '\r') {
                newlinesSeen = 0;
                if (!headerDone && replyLen < reply.length) {
                    reply[replyLen++] = (byte) i;
                }
            }
        }
        /*
         * Converting the byte array to a string is slightly wasteful
         * in the case where the connection was successful, but it's
         * insignificant compared to the network overhead.
         */
        String replyStr;
        try {
            replyStr = new String(reply, 0, replyLen, "ASCII7");
        } catch (UnsupportedEncodingException ignored) {
            replyStr = new String(reply, 0, replyLen);
        }

        /* We asked for HTTP/1.0, so we should get that back */
        if (!replyStr.startsWith("HTTP/1.0 200") && !replyStr.startsWith("HTTP/1.1 200")) {
            throw new IOException("Unable to tunnel through " + host + ":" + port + ".  Proxy returns \"" + replyStr + "\"");
        }
    }

    public static SSLSocketFactory getUnsafeSslSocketFactory() {
        try {
            final TrustManager[] trustAllCerts = new TrustManager[]{new X509TrustManager() {
                @Override
                public void checkClientTrusted(java.security.cert.X509Certificate[] chain, String authType) {
                }

                @Override
                public void checkServerTrusted(java.security.cert.X509Certificate[] chain, String authType) {
                }

                @Override
                public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                    return new java.security.cert.X509Certificate[]{};
                }
            }};

            SSLContext sslContext = SSLContext.getInstance("SSL");
            sslContext.init(null, trustAllCerts, new java.security.SecureRandom());

            return sslContext.getSocketFactory();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
