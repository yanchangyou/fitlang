
cd /d %~dp0

java -classpath lib/instrumented-fitlang-0.10.11.jar;lib/slf4j-api-2.0.9.jar;lib/slf4j-jdk14-2.0.9.jar;lib/jul-to-slf4j-2.0.9.jar;lib/oshi-core-6.4.7.jar;lib/jna-platform-5.13.0.jar;lib/jna-5.13.0.jar;lib/hutool-all-5.8.25.jar;lib/fastjson2-2.0.46.jar;lib/mvel2-2.5.1.Final.jar fit.server.FitServerMain
