package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeEngineConst;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class ConvertJsonExecuteNodeTestBiz extends TestCase {

    public void testExecute100() {
        String input = "{\n" +
                "\t\"clients\": [\n" +
                "\t\t{\n" +
                "\t\t\t\"sessionId\": \"03ae2bdf-c36b-48a3-a402-daca9a5870d0\",\n" +
                "\t\t\t\"connectTime\": \"2023-09-17 13:27:47\",\n" +
                "\t\t\t\"clientId\": \"127.0.0.1\",\n" +
                "\t\t\t\"info\": {\n" +
                "\t\t\t\t\"message\": \"ok\",\n" +
                "\t\t\t\t\"meta\": {\n" +
                "\t\t\t\t\t\"sessionId\": \"4e317371-35f8-451e-88f9-9855ae33c075\"\n" +
                "\t\t\t\t},\n" +
                "\t\t\t\t\"userInfo\": {\n" +
                "\t\t\t\t\t\"message\": \"ok\",\n" +
                "\t\t\t\t\t\"meta\": {\n" +
                "\t\t\t\t\t\t\"sessionId\": \"4e317371-35f8-451e-88f9-9855ae33c075\"\n" +
                "\t\t\t\t\t},\n" +
                "\t\t\t\t\t\"userName\": \"coder\"\n" +
                "\t\t\t\t},\n" +
                "\t\t\t\t\"clientInfo\": {\n" +
                "\t\t\t\t\t\"message\": \"ok\",\n" +
                "\t\t\t\t\t\"meta\": {\n" +
                "\t\t\t\t\t\t\"sessionId\": \"4e317371-35f8-451e-88f9-9855ae33c075\"\n" +
                "\t\t\t\t\t},\n" +
                "\t\t\t\t\t\"clientType\": \"java\"\n" +
                "\t\t\t\t},\n" +
                "\t\t\t\t\"systemInfo\": {\n" +
                "\t\t\t\t\t\"computerManufacturer\": \"Apple Inc.\"\n" +
                "\t\t\t\t}\n" +
                "\t\t\t}\n" +
                "\t\t}\n" +
                "\t]\n" +
                "}";
        String flow = "{\n" +
                "    \"uni\": \"pipe\",\n" +
                "    \"child\": [\n" +
                "        {\n" +
                "            \"uni\": \"mix\",\n" +
                "            \"json\": " + input +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"convert\",\n" +
                "            \"needCloneInputData\": true,\n" +
                "            \"express\": {\n" +
                "                \"clients[].clientId\": \"${clients[].clientId}\",\n" +
                "                \"clients[].sessionId\": \"${clients[].sessionId}\",\n" +
                "                \"clients[].connectTime\": \"${clients[].connectTime}\"\n" +
                "            }\n" +
                "        }\n" +
                "    ]\n" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("clients"));
        Assert.assertEquals("127.0.0.1", ((JSONObject) (outputJson.getJSONArray("clients").get(0))).get("clientId"));

    }

}