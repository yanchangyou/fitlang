
@set httpPrefix=%1

cd /d %~dp0
cd ..

java -classpath lib/fitlang-0.7.5.jar;lib/slf4j-api-2.0.9.jar;lib/slf4j-jdk14-2.0.9.jar;lib/jul-to-slf4j-2.0.9.jar;lib/oshi-core-6.4.7.jar;lib/jna-platform-5.13.0.jar;lib/jna-5.13.0.jar;lib/hutool-all-5.8.23.jar;lib/fastjson2-2.0.42.jar;lib/aviator-5.3.3.jar fit.server.FitServerMain app/server.fit %httpPrefix%
