
httpPrefix=$FIT_SERVER_HTTP_PREFIX
fitPath=$FIT_PATH

cd "$(dirname $0)/"

#curl http://127.0.0.1:11111/_stop
curl http://127.0.0.1:11111/_shutdown

sleep 0.5

jdk/bin/java -classpath lib/fitlang-0.7.2.jar:\
lib/hutool-all-5.8.21.jar:\
lib/aviator-5.3.3.jar:\
lib/jna-5.13.0.jar:\
lib/jna-platform-5.13.0.jar:\
lib/oshi-core-6.4.5.jar:\
lib/fastjson2-2.0.32.jar:\
lib/slf4j-api-2.0.7.jar:\
 fit.server.FitServerMain server.fit $httpPrefix $fitPath&
