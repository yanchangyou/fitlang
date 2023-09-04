package fit.lang.plugin.json.cloud;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import static fit.lang.plugin.json.ExpressUtil.eval;


/**
 * 使用http的长连接实现
 * 长连接实现选项 https://www.dandelioncloud.cn/article/details/1599068952804409346
 */
public class CloudServerJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {

        String registerUrl = nodeJsonDefine.getString("registerUrl");

        registerUrl = (String) eval(registerUrl, input.getInputParamAndContextParam());

        execute(registerUrl);
    }

    private static void execute(String registerUrl) {
        URL remoteUrl = null;
        try {
            remoteUrl = new URL(registerUrl);
        } catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }

        OutputStream outputStream;
        InputStream inputStream;
        URLConnection urlConnection;
        try {
            urlConnection = remoteUrl.openConnection();
            urlConnection.setDoOutput(true);
            outputStream = urlConnection.getOutputStream();

            inputStream = urlConnection.getInputStream();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        byte[] data = new byte[1024];
        StringBuilder stringBuilder = new StringBuilder();
        while (true) {
            try {
                int length = inputStream.read(data);
                if (length != -1) {
                    stringBuilder.append(new String(data, 0, length));
                    int endIndex = stringBuilder.indexOf("}{$?!}");
                    if (endIndex > -1) {
                        String code = stringBuilder.substring(0, endIndex + 1);

                        System.out.println("code" + code);

                        JSONObject fitInput = JSONObject.parse(code);

                        JSONObject defaultInput = new JSONObject();
                        if (fitInput.containsKey("input")) {
                            defaultInput = fitInput.getJSONObject("input");
                        }

                        JSONObject inputJson = defaultInput;

                        String result = ExecuteJsonNodeUtil.executeCode(inputJson, fitInput);
                        System.out.println("result:" + result);
                        outputStream.write(result.getBytes());
                        outputStream.write("{$?!}".getBytes());
                        outputStream.flush();

                        stringBuilder = new StringBuilder(stringBuilder.substring(endIndex + 6));

                    }
                } else {
                    break;
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }

}