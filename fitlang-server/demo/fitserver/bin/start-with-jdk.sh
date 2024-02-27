
httpPrefix=$FIT_SERVER_HTTP_PREFIX
fitPath=$FIT_PATH

cd "$(dirname $0)/.."

#curl http://127.0.0.1:11111/_stop
curl http://127.0.0.1:11111/_shutdown

sleep 0.5

jdk/bin/java -classpath lib/instrumented-fitlang-0.10.15.jar:\
lib/hutool-all-5.8.25.jar:\
lib/mvel2-2.5.1.Final.jar:\
lib/jna-5.13.0.jar:\
lib/jna-platform-5.13.0.jar:\
lib/oshi-core-6.4.7.jar:\
lib/fastjson2-2.0.46.jar:\
lib/slf4j-api-2.0.9.jar:\
lib/slf4j-jdk14-2.0.9.jar:\
lib/jul-to-slf4j-2.0.9.jar:\
 fit.server.FitServerMain server.fit $httpPrefix $fitPath

