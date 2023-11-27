
httpPrefix=$FIT_SERVER_HTTP_PREFIX
fitPath=$FIT_PATH

cd "$(dirname $0)/.."

#curl http://127.0.0.1:11111/_stop
curl http://127.0.0.1:11111/_shutdown

sleep 0.5

java -classpath lib/fitlang-0.7.4.jar:\
lib/hutool-all-5.8.23.jar:\
lib/aviator-5.3.3.jar:\
lib/jna-5.13.0.jar:\
lib/jna-platform-5.13.0.jar:\
lib/oshi-core-6.4.7.jar:\
lib/fastjson2-2.0.42.jar:\
lib/slf4j-api-2.0.9.jar:\
lib/jcl-over-slf4j-2.0.9.jar:\
 fit.server.FitServerMain server.fit $httpPrefix $fitPath&

