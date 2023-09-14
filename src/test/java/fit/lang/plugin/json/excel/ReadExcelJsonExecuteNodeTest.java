package fit.lang.plugin.json.excel;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class ReadExcelJsonExecuteNodeTest extends TestCase {

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        String path = WriteExcelJsonExecuteNodeTest.class.getResource(".").getFile();
        ServerJsonExecuteNode.setCurrentServerFilePath(path.replace("/test/classes/fit", "/test/resources/fit"));
        System.out.println(ServerJsonExecuteNode.getServerFileDir());
    }

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'readExcel'," +
                "   'titleIndex': 1," +
                "   'path': '测试用例.xls'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertNotNull(outputJson.get("list"));

    }

    public void testExecute2() {
        String flow = "{" +//
                "   'uni': 'readExcel'," +
                "   'titleIndex': 1," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{ 'path': '测试用例.xls'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertNotNull(outputJson.get("list"));

    }

    public void testExecute3() {
        String flow = "{" +//
                "   'uni': 'readExcel'," +
                "   'titleIndex': 0," +
                "   'path': '测试用例.xls'" +
                "}";
        try {
            ExecuteJsonNodeUtil.executeCode("{}", flow);
        } catch (Exception e) {
            e.printStackTrace();
            Assert.assertTrue(e.getMessage().contains("excel titleIndex must be great than 0, but found: 0"));
        }
    }

    public void testExecute5() {
        String flow = "{" +//
                "   'uni': 'readExcel'," +
                "   'titleIndex': 1," +
                "   'sheetName': '接口列表'," +
                "   'path': '测试用例.xls'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertNotNull(outputJson.get("list"));

    }

    public void testExecute6() {
        String flow = "{" +//
                "   'uni': 'readExcel'," +
                "   'outputListField': 'otherName'," +
                "   'titleIndex': 1," +
                "   'path': '测试用例.xls'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertNotNull(outputJson.get("otherName"));

    }

    public void testExecute100() {

        String flow = "{" +
                "    'uni': 'sequence'," +
                "    'child': [" +
                "        {" +
                "            'uni': 'server'," +
                "            'port': 60002," +
                "            'service': {'/hello':{'uni':'hello'}}," +
                "        }," +
                "        {" +
                "            'flag': '_needFormatJsonInConsole'," +
                "            'uni': 'pipe'," +
                "            'child': [" +
                "                {" +
                "                    'uni': 'mix'," +
                "                    'json': {" +
                "                        'path': '测试用例.xls'," +
                "                        'sheetName': '接口列表'" +
                "                    }" +
                "                }," +
                "                {" +
                "                    'uni': 'readExcel'," +
                "                    'titleIndex': 1" +
                "                }," +
                "                {" +
                "                    'uni': 'foreach'," +
                "                    'foreachField': 'list'," +
                "                    'child': {" +
                "                        'uni': 'pipe'," +
                "                        'child': [" +
                "                            {" +
                "                                'uni': 'set'," +
                "                                'key': 'url'," +
                "                                'value': '${接口URL}'" +
                "                            }," +
                "                            {" +
                "                                'uni': 'mix'," +
                "                                'json': {" +
                "                                    'path': '测试用例.xls'," +
                "                                    'sheetName': '${接口名称}'" +
                "                                }" +
                "                            }," +
                "                            {" +
                "                                'uni': 'readExcel'," +
                "                                'titleIndex': 1" +
                "                            }," +
                "                            {" +
                "                                'uni': 'foreach'," +
                "                                'foreachField': 'list'," +
                "                                'child': {" +
                "                                    'uni': 'pipe'," +
                "                                    'child': [" +
                "                                        {" +
                "                                            'uni': 'set'," +
                "                                            'key': 'expectedValue'," +
                "                                            'value': '${期望}'" +
                "                                        }," +
                "                                        {" +
                "                                            'uni': 'mix'," +
                "                                            'json': {" +
                "                                                'url': '${url}'," +
                "                                                'expectedValue': '${expectedValue}'" +
                "                                            }" +
                "                                        }," +
                "                                        {" +
                "                                            'uni': 'http'," +
                "                                            'header': {" +
                "                                                'contextType': 'application/x-www-form-urlencoded'" +
                "                                            }," +
                "                                            'param': '${输入}'" +
                "                                        }," +
                "                                        {" +
                "                                            'uni': 'assert'," +
                "                                            'needToString': true," +
                "                                            'expected': '${expectedValue}'" +
                "                                        }" +
                "                                    ]" +
                "                                }" +
                "                            }" +
                "                        ]" +
                "                    }" +
                "                }" +
                "            ]" +
                "        }" +
                "    ]" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertTrue(!output.isEmpty());


    }


    public void testExecute101() {

        String flow = "{\n" +
                "    \"flag\": \"_needFormatJsonInConsole\",\n" +
                "    \"uni\": \"pipe\",\n" +
                "    \"input\": {\n" +
                "        \"path\": \"测试用例.xls\",\n" +
                "        \"sheetName\": \"接口列表\"\n" +
                "    },\n" +
                "    \"child\": [\n" +
                "        {\n" +
                "            \"uni\": \"readExcel\"\n" +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"foreach\",\n" +
                "            \"foreachField\": \"list\",\n" +
                "            \"child\": {\n" +
                "                \"uni\": \"pipe\",\n" +
                "                \"child\": [\n" +
                "                    {\n" +
                "                        \"uni\": \"set\",\n" +
                "                        \"key\": \"apiItem\",\n" +
                "                        \"value\": {\n" +
                "                            \"url\": \"${接口URL}\",\n" +
                "                            \"name\": \"${接口名称}\"\n" +
                "                        }\n" +
                "                    },\n" +
                "                    {\n" +
                "                        \"uni\": \"readExcel\",\n" +
                "                        \"path\": \"${input.path}\",\n" +
                "                        \"sheetName\": \"${apiItem.name}\"\n" +
                "                    },\n" +
                "                    {\n" +
                "                        \"uni\": \"foreach\",\n" +
                "                        \"foreachField\": \"list\",\n" +
                "                        \"child\": {\n" +
                "                            \"uni\": \"pipe\",\n" +
                "                            \"child\": [\n" +
                "                                {\n" +
                "                                    \"uni\": \"set\",\n" +
                "                                    \"key\": \"caseItem\",\n" +
                "                                    \"value\": {\n" +
                "                                        \"编码\": \"${编码}\",\n" +
                "                                        \"名称\": \"${名称}\",\n" +
                "                                        \"输入\": \"${输入}\",\n" +
                "                                        \"期望\": \"${期望}\"\n" +
                "                                    }\n" +
                "                                },\n" +
                "                                {\n" +
                "                                    \"uni\": \"mix\",\n" +
                "                                    \"json\": {\n" +
                "                                        \"url\": \"${apiItem.url}\",\n" +
                "                                        \"expectedValue\": \"${期望}\"\n" +
                "                                    }\n" +
                "                                },\n" +
                "                                {\n" +
                "                                    \"uni\": \"http\",\n" +
                "                                    \"param\": \"${输入}\"\n" +
                "                                },\n" +
                "                                {\n" +
                "                                    \"uni\": \"assert\",\n" +
                "                                    \"needToString\": true,\n" +
                "                                    \"expected\": \"${caseItem.期望}\"\n" +
                "                                },\n" +
                "                                {\n" +
                "                                    \"uni\": \"mix\",\n" +
                "                                    \"pickJsonField\": \"caseItem\",\n" +
                "                                    \"json\": {\n" +
                "                                        \"caseItem\": \"${caseItem}\"\n" +
                "                                    }\n" +
                "                                }\n" +
                "                            ]\n" +
                "                        }\n" +
                "                    },\n" +
                "                    {\n" +
                "                        \"uni\": \"writeExcel\",\n" +
                "                        \"path\": \"${'测试报告-'+System.currentTimeMillis()+'.xls'}\",\n" +
                "                        \"titleConfig\": {\n" +
                "                            \"编码\": {\n" +
                "                                \"title\": \"编码\"\n" +
                "                            },\n" +
                "                            \"名称\": {\n" +
                "                                \"title\": \"名称\"\n" +
                "                            },\n" +
                "                            \"输入\": {\n" +
                "                                \"title\": \"输入\"\n" +
                "                            },\n" +
                "                            \"期望\": {\n" +
                "                                \"title\": \"期望\"\n" +
                "                            },\n" +
                "                            \"success\": {\n" +
                "                                \"title\": \"是否通过\"\n" +
                "                            },\n" +
                "                            \"actual\": {\n" +
                "                                \"title\": \"实际输出\"\n" +
                "                            }\n" +
                "                        }\n" +
                "                    }\n" +
                "                ]\n" +
                "            }\n" +
                "        }\n" +
                "    ]\n" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertTrue(!output.isEmpty());


    }

    public void testExecute102() {

        String flow = "{\n" +
                "    \"flag\": \"_needFormatJsonInConsole\",\n" +
                "    \"uni\": \"pipe\",\n" +
                "    \"input\": {\n" +
                "        \"path\": \"测试用例.xls\",\n" +
                "        \"sheetName\": \"接口列表\",\n" +
                "        \"testRepostExcelName\": \"${'测试报告-'+System.currentTimeMillis()+'.xls'}\"\n" +
                "    },\n" +
                "    \"child\": [\n" +
                "        {\n" +
                "            \"uni\": \"readExcel\"\n" +
                "        },\n" +
                "        {\n" +
                "            \"uni\": \"foreach\",\n" +
                "            \"foreachField\": \"list\",\n" +
                "            \"child\": {\n" +
                "                \"uni\": \"pipe\",\n" +
                "                \"child\": [\n" +
                "                    {\n" +
                "                        \"description\": \"先放入全局变量中，后好获取\",\n" +
                "                        \"uni\": \"set\",\n" +
                "                        \"key\": \"apiItem\",\n" +
                "                        \"value\": {\n" +
                "                            \"url\": \"${接口URL}\",\n" +
                "                            \"name\": \"${接口名称}\"\n" +
                "                        }\n" +
                "                    },\n" +
                "                    {\n" +
                "                        \"uni\": \"readExcel\",\n" +
                "                        \"path\": \"${input.path}\",\n" +
                "                        \"sheetName\": \"${apiItem.name}\"\n" +
                "                    },\n" +
                "                    {\n" +
                "                        \"uni\": \"foreach\",\n" +
                "                        \"foreachField\": \"list\",\n" +
                "                        \"child\": {\n" +
                "                            \"uni\": \"pipe\",\n" +
                "                            \"child\": [\n" +
                "                                {\n" +
                "                                    \"uni\": \"set\",\n" +
                "                                    \"key\": \"caseItem\",\n" +
                "                                    \"value\": {\n" +
                "                                        \"编码\": \"${编码}\",\n" +
                "                                        \"名称\": \"${名称}\",\n" +
                "                                        \"输入\": \"${输入}\",\n" +
                "                                        \"期望\": \"${期望}\"\n" +
                "                                    }\n" +
                "                                },\n" +
                "                                {\n" +
                "                                    \"uni\": \"http\",\n" +
                "                                    \"param\": \"${输入}\",\n" +
                "                                    \"url\": \"${apiItem.url}\"\n" +
                "                                },\n" +
                "                                {\n" +
                "                                    \"uni\": \"assert\",\n" +
                "                                    \"needToString\": true,\n" +
                "                                    \"expected\": \"${caseItem.期望}\"\n" +
                "                                },\n" +
                "                                {\n" +
                "                                    \"uni\": \"mix\",\n" +
                "                                    \"pickJsonField\": \"caseItem\",\n" +
                "                                    \"json\": {\n" +
                "                                        \"caseItem\": \"${caseItem}\"\n" +
                "                                    }\n" +
                "                                }\n" +
                "                            ]\n" +
                "                        }\n" +
                "                    },\n" +
                "                    {\n" +
                "                        \"uni\": \"writeExcel\",\n" +
                "                        \"path\": \"${input.testRepostExcelName}\",\n" +
                "                        \"titleConfig\": {\n" +
                "                            \"编码\": \"编码\",\n" +
                "                            \"名称\": \"名称\",\n" +
                "                            \"输入\": \"输入\",\n" +
                "                            \"期望\": \"期望\",\n" +
                "                            \"success\": \"是否通过\",\n" +
                "                            \"actual\": \"实际输出\"\n" +
                "                        }\n" +
                "                    }\n" +
                "                ]\n" +
                "            }\n" +
                "        }\n" +
                "    ]\n" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertTrue(!output.isEmpty());


    }
}
