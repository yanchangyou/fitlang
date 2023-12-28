package fit.server;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.function.JsonPackageExecuteNode;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;

import java.io.File;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.readNodeDefineFile;

public class FitServerMain {

    public static void main(String[] args) {

        System.out.println("FitLang-0.9.0");
        String fitPath = "app";
        String serverFilePath = "server.fit";
        String httpPrefix = "http://127.0.0.1";
        int port = 11111;

        if (args != null) {
            if (args.length > 0) {
                serverFilePath = args[0];
            }
            if (args.length > 1) {
                httpPrefix = args[1];
            }
            if (args.length > 2) {
                fitPath = args[2];
            }
        }
        File serverFile = new File(fitPath.concat("/").concat(serverFilePath));

        if (!serverFile.exists()) {
            System.out.println("server file not existed: " + serverFile.getAbsoluteFile());
            return;
        }

        String code = readNodeDefineFile(serverFile);

        System.out.println("start server from " + serverFile.getAbsoluteFile());

        ServerJsonExecuteNode.setCurrentServerFilePath(serverFile.getAbsolutePath());
        ServerJsonExecuteNode.setHttpPrefix(httpPrefix);
        JsonPackageExecuteNode.addImportPath(ServerJsonExecuteNode.getServerFileDir());

        String result = ExecuteJsonNodeUtil.executeCode(code);
        System.out.println(result);

        System.out.println("start OK!");

        JSONObject resultJson;
        if (isJsonObjectText(result)) {
            resultJson = JSON.parseObject(result);
            port = resultJson.getInteger("port");
            httpPrefix = resultJson.getString("httpPrefix");
        }

        System.out.println(httpPrefix + ":" + port);

    }
}
