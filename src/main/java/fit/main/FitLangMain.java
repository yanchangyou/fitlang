package fit.main;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.CharsetUtil;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.tool.ServerJsonExecuteNode;

import java.io.File;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonText;

public class FitLangMain {

    public static void main(String[] args) {

        System.out.println("FitLang-0.4.6");
        String serverFilePath = "server.fit";

        if (args != null && args.length > 0) {
            serverFilePath = args[0];
        }
        File serverFile = new File(serverFilePath);

        if (!serverFile.exists()) {
            System.out.println("server file not existed: " + serverFile.getAbsoluteFile());
            return;
        }

        String code = FileUtil.readString((serverFile), CharsetUtil.defaultCharset());

        System.out.println("start server: " + serverFile.getAbsoluteFile());

        ServerJsonExecuteNode.setCurrentServerFilePath(serverFile.getAbsolutePath());

        String result = ExecuteJsonNodeUtil.executeCode(code);
        System.out.println(result);

        System.out.println("start OK!");

        JSONObject resultJson;
        int port = 11111;
        String host = "127.0.0.1";
        if (isJsonText(result)) {
            resultJson = JSON.parseObject(result);
            port = resultJson.getInteger("port");
            host = resultJson.getString("host");
        }

        System.out.println("http://" + host + ":" + port + "/_api");

    }
}
