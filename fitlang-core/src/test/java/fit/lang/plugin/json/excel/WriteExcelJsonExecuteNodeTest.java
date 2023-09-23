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
        ServerJsonExecuteNode.setCurrentServerFilePath(
                path.replace("/test/classes/fit", "/test/resources/fit")
                        .replace("/classes/java/test-instrumented/fit", "/resources/test/fit"));
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

        String flow = "{" +
                "    'uni': 'sequence'," +
                "    'child': [" +
                "        {" +
                "            'uni': 'server'," +
                "            'port': 60002," +
                "            'service': {" +
                "                '/hello': {" +
                "                    'uni': 'hello'" +
                "                }," +
                "                '/echo': {" +
                "                    'uni': 'echo'" +
                "                }" +
                "            }" +
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
                "                                            'key': 'caseItem'," +
                "                                            'value': {" +
                "                                                '编码': '${编码}'," +
                "                                                '名称': '${名称}'," +
                "                                                '输入': '${输入}'," +
                "                                                '期望': '${期望}'" +
                "                                            }" +
                "                                        }," +
                "                                        {" +
                "                                            'uni': 'mix'," +
                "                                            'json': {" +
                "                                                'url': '${url}'," +
                "                                                'expectedValue': '${期望}'" +
                "                                            }" +
                "                                        }," +
                "                                        {" +
                "                                            'uni': 'http'," +
                "                                            'param': '${输入}'" +
                "                                        }," +
                "                                        {" +
                "                                            'uni': 'assert'," +
                "                                            'needToString': true," +
                "                                            'expected': '${caseItem.期望}'" +
                "                                        }," +
                "                                        {" +
                "                                            'uni': 'mix'," +
                "                                            'pickJsonField': 'caseItem'," +
                "                                            'json': {" +
                "                                                'caseItem': '${caseItem}'" +
                "                                            }" +
                "                                        }" +
                "                                    ]" +
                "                                }" +
                "                            }," +
                "                            {" +
                "                                'uni': 'writeExcel'," +
                "                                'path': '测试报告.xls'," +
                "                                'titleConfig': {" +
                "                                    '编码': {" +
                "                                        'title': '编码'" +
                "                                    }," +
                "                                    '名称': {" +
                "                                        'title': '名称'" +
                "                                    }," +
                "                                    '输入': {" +
                "                                        'title': '输入'" +
                "                                    }," +
                "                                    '期望': {" +
                "                                        'title': '期望'" +
                "                                    }," +
                "                                    'success': {" +
                "                                        'title': '是否通过'" +
                "                                    }," +
                "                                    'actual': {" +
                "                                        'title': '实际输出'" +
                "                                    }" +
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
}