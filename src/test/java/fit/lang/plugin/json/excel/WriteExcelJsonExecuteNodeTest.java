package fit.lang.plugin.json.excel;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class WriteExcelJsonExecuteNodeTest extends TestCase {

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        String path = WriteExcelJsonExecuteNodeTest.class.getResource(".").getFile();
        ServerJsonExecuteNode.setCurrentServerFilePath(path.replace("/test/classes/fit","/test/resources/fit"));
        System.out.println(ServerJsonExecuteNode.getServerFileDir());
    }

    public void testExecute() {

        String flow = "{" +//
                "   'uni': 'writeExcel'," +
                "   'titleIndex': 1," +
                "   'titleConfig': {'a':{'title':'列名1'}}," +
                "   'path': 'excel.xls'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'list':[{'a':1},{'a':2}]}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertNotNull(outputJson.get("list"));

    }

    public void testExecute2() {

        String flow = "{\n" +
                "    \"uni\": \"sequence\",\n" +
                "    \"child\": [\n" +
                "        {\n" +
                "            \"uni\": \"server\",\n" +
                "            \"port\": 60001,\n" +
                "            \"service\": {\n" +
                "                \"/hello\": {\n" +
                "                    \"uni\": \"hello\"\n" +
                "                },\n" +
                "                \"/echo\": {\n" +
                "                    \"uni\": \"echo\"\n" +
                "                }\n" +
                "            }\n" +
                "        },\n" +
                "        {\n" +
                "            \"flag\": \"_needFormatJsonInConsole\",\n" +
                "            \"uni\": \"pipe\",\n" +
                "            \"child\": [\n" +
                "                {\n" +
                "                    \"uni\": \"mix\",\n" +
                "                    \"json\": {\n" +
                "                        \"path\": \"testcase.xls\",\n" +
                "                        \"sheetName\": \"接口列表\"\n" +
                "                    }\n" +
                "                },\n" +
                "                {\n" +
                "                    \"uni\": \"readExcel\",\n" +
                "                    \"titleIndex\": 1\n" +
                "                },\n" +
                "                {\n" +
                "                    \"uni\": \"foreach\",\n" +
                "                    \"foreachField\": \"list\",\n" +
                "                    \"child\": {\n" +
                "                        \"uni\": \"pipe\",\n" +
                "                        \"child\": [\n" +
                "                            {\n" +
                "                                \"uni\": \"set\",\n" +
                "                                \"key\": \"url\",\n" +
                "                                \"value\": \"${接口URL}\"\n" +
                "                            },\n" +
                "                            {\n" +
                "                                \"uni\": \"mix\",\n" +
                "                                \"json\": {\n" +
                "                                    \"path\": \"testcase.xls\",\n" +
                "                                    \"sheetName\": \"${接口名称}\"\n" +
                "                                }\n" +
                "                            },\n" +
                "                            {\n" +
                "                                \"uni\": \"readExcel\",\n" +
                "                                \"titleIndex\": 1\n" +
                "                            },\n" +
                "                            {\n" +
                "                                \"uni\": \"foreach\",\n" +
                "                                \"foreachField\": \"list\",\n" +
                "                                \"child\": {\n" +
                "                                    \"uni\": \"pipe\",\n" +
                "                                    \"child\": [\n" +
                "                                        {\n" +
                "                                            \"uni\": \"set\",\n" +
                "                                            \"key\": \"编码\",\n" +
                "                                            \"value\": \"${编码}\"\n" +
                "                                        },\n" +
                "                                        {\n" +
                "                                            \"uni\": \"set\",\n" +
                "                                            \"key\": \"名称\",\n" +
                "                                            \"value\": \"${名称}\"\n" +
                "                                        },\n" +
                "                                        {\n" +
                "                                            \"uni\": \"set\",\n" +
                "                                            \"key\": \"期望\",\n" +
                "                                            \"value\": \"${期望}\"\n" +
                "                                        },\n" +
                "                                        {\n" +
                "                                            \"uni\": \"set\",\n" +
                "                                            \"key\": \"输入\",\n" +
                "                                            \"value\": \"${输入}\"\n" +
                "                                        },\n" +
                "                                        {\n" +
                "                                            \"uni\": \"mix\",\n" +
                "                                            \"json\": {\n" +
                "                                                \"url\": \"${url}\",\n" +
                "                                                \"expectedValue\": \"${期望}\"\n" +
                "                                            }\n" +
                "                                        },\n" +
                "                                        {\n" +
                "                                            \"uni\": \"http\",\n" +
                "                                            \"param\": \"${输入}\"\n" +
                "                                        },\n" +
                "                                        {\n" +
                "                                            \"uni\": \"assert\",\n" +
                "                                            \"needToString\": true,\n" +
                "                                            \"expected\": \"${期望}\"\n" +
                "                                        },\n" +
                "                                        {\n" +
                "                                            \"uni\": \"mix\",\n" +
                "                                            \"json\": {\n" +
                "                                                \"编码\": \"${编码}\",\n" +
                "                                                \"名称\": \"${名称}\",\n" +
                "                                                \"输入\": \"${输入}\",\n" +
                "                                                \"期望\": \"${期望}\",\n" +
                "                                                \"是否匹配\": \"${success}\",\n" +
                "                                                \"实际\": \"${actual}\"\n" +
                "                                            }\n" +
                "                                        }\n" +
                "                                    ]\n" +
                "                                }\n" +
                "                            },\n" +
                "                            {\n" +
                "                                \"uni\": \"writeExcel\",\n" +
                "                                \"path\": \"testcase-report.xls\",\n" +
                "                                \"titleConfig\": {\n" +
                "                                    \"编码\": {\n" +
                "                                        \"title\": \"编码\"\n" +
                "                                    },\n" +
                "                                    \"名称\": {\n" +
                "                                        \"title\": \"名称\"\n" +
                "                                    },\n" +
                "                                    \"输入\": {\n" +
                "                                        \"title\": \"输入\"\n" +
                "                                    },\n" +
                "                                    \"期望\": {\n" +
                "                                        \"title\": \"期望\"\n" +
                "                                    },\n" +
                "                                    \"success\": {\n" +
                "                                        \"title\": \"是否通过\"\n" +
                "                                    },\n" +
                "                                    \"actual\": {\n" +
                "                                        \"title\": \"实际输出\"\n" +
                "                                    }\n" +
                "                                }\n" +
                "                            }\n" +
                "                        ]\n" +
                "                    }\n" +
                "                }\n" +
                "            ]\n" +
                "        }\n" +
                "    ]\n" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertTrue(!output.isEmpty());

    }
}