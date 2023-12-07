package fit.lang.plugin.json.net;

import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
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
//            doTunnelHandshake(proxySocket, host, port);

            socket = (SSLSocket) factory.createSocket(proxySocket, host, port, true);
        } else {
            socket = (SSLSocket) factory.createSocket(host, port);
        }
        socket.startHandshake();
        return socket;
    }

//    /*
//     * Tell our tunnel where we want to CONNECT, and look for the
//     * right reply.  Throw IOException if anything goes wrong.
//     */
//    private static void doTunnelHandshake(Socket tunnel, String host, int port)
//            throws IOException {
//        OutputStream out = tunnel.getOutputStream();
//        String msg = "CONNECT " + host + ":" + port + " HTTP/1.0\n"
//                + "\r\n\r\n";
//        byte b[];
//        try {
//            /*
//             * We really do want ASCII7 -- the http protocol doesn't change
//             * with locale.
//             */
//            b = msg.getBytes("ASCII7");
//        } catch (UnsupportedEncodingException ignored) {
//            /*
//             * If ASCII7 isn't there, something serious is wrong, but
//             * Paranoia Is Good (tm)
//             */
//            b = msg.getBytes();
//        }
//        out.write(b);
//        out.flush();
//
//        /*
//         * We need to store the reply so we can create a detailed
//         * error message to the user.
//         */
//        byte reply[] = new byte[200];
//        int replyLen = 0;
//        int newlinesSeen = 0;
//        boolean headerDone = false;     /* Done on first newline */
//
//        InputStream in = tunnel.getInputStream();
//        boolean error = false;
//
//        while (newlinesSeen < 2) {
//            int i = in.read();
//            if (i < 0) {
//                throw new IOException("Unexpected EOF from proxy");
//            }
//            if (i == '\n') {
//                headerDone = true;
//                ++newlinesSeen;
//            } else if (i != '\r') {
//                newlinesSeen = 0;
//                if (!headerDone && replyLen < reply.length) {
//                    reply[replyLen++] = (byte) i;
//                }
//            }
//        }
//
//        /*
//         * Converting the byte array to a string is slightly wasteful
//         * in the case where the connection was successful, but it's
//         * insignificant compared to the network overhead.
//         */
//        String replyStr;
//        try {
//            replyStr = new String(reply, 0, replyLen, "ASCII7");
//        } catch (UnsupportedEncodingException ignored) {
//            replyStr = new String(reply, 0, replyLen);
//        }
//
//        /* We asked for HTTP/1.0, so we should get that back */
//        if (!replyStr.startsWith("HTTP/1.0 200") && !replyStr.startsWith("HTTP/1.1 200")) {
//            throw new IOException("Unable to tunnel through "
////                    + tunnelHost + ":" + tunnelPort
//                    + ".  Proxy returns \"" + replyStr + "\"");
//        }
//
//    }
}
