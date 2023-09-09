cd "$(dirname $0)/.."

curl http://127.0.0.1:11111/_stop

sleep 0.5

java -classpath lib/fitlang-0.4.6.jar:\
lib/hutool-all-5.8.21.jar:\
lib/aviator-5.3.3.jar:\
lib/hutool-all-5.8.21.jar:\
lib/netty-codec-http-4.1.97.Final.jar:\
lib/netty-handler-proxy-4.1.97.Final.jar:\
lib/netty-transport-native-epoll-4.1.97.Final-linux-x86_64.jar:\
lib/fastjson2-2.0.32.jar:\
lib/netty-codec-http2-4.1.97.Final.jar:\
lib/netty-handler-ssl-ocsp-4.1.97.Final.jar:\
lib/netty-transport-native-kqueue-4.1.97.Final-osx-aarch_64.jar:\
lib/netty-codec-memcache-4.1.97.Final.jar:\
lib/netty-resolver-4.1.97.Final.jar:\
lib/netty-transport-native-kqueue-4.1.97.Final-osx-x86_64.jar:\
lib/netty-codec-mqtt-4.1.97.Final.jar:\
lib/netty-resolver-dns-4.1.97.Final.jar:\
lib/netty-transport-native-unix-common-4.1.97.Final.jar:\
lib/jna-5.13.0.jar:\
lib/netty-codec-redis-4.1.97.Final.jar:\
lib/netty-resolver-dns-classes-macos-4.1.97.Final.jar:\
lib/netty-transport-rxtx-4.1.97.Final.jar:\
lib/jna-platform-5.13.0.jar:\
lib/netty-codec-smtp-4.1.97.Final.jar:\
lib/netty-resolver-dns-native-macos-4.1.97.Final-osx-aarch_64.jar:\
lib/netty-transport-sctp-4.1.97.Final.jar:\
lib/netty-all-4.1.97.Final.jar:\
lib/netty-codec-socks-4.1.97.Final.jar:\
lib/netty-resolver-dns-native-macos-4.1.97.Final-osx-x86_64.jar:\
lib/netty-transport-udt-4.1.97.Final.jar:\
lib/netty-buffer-4.1.97.Final.jar:\
lib/netty-codec-stomp-4.1.97.Final.jar:\
lib/netty-transport-4.1.97.Final.jar:\
lib/oshi-core-6.4.5.jar:\
lib/netty-codec-4.1.97.Final.jar:\
lib/netty-codec-xml-4.1.97.Final.jar:\
lib/netty-transport-classes-epoll-4.1.97.Final.jar:\
lib/searchableOptions-0.4.6.jar:\
lib/netty-codec-dns-4.1.97.Final.jar:\
lib/netty-common-4.1.97.Final.jar:\
lib/netty-transport-classes-kqueue-4.1.97.Final.jar:\
lib/slf4j-api-2.0.7.jar:\
lib/netty-codec-haproxy-4.1.97.Final.jar:\
lib/netty-handler-4.1.97.Final.jar:\
lib/netty-transport-native-epoll-4.1.97.Final-linux-aarch_64.jar\
 fit.server.FitServerMain app/server.fit $FIT_SERVER_HTTP_PREFIX&

