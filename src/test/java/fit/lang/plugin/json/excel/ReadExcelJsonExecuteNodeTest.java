package fit.lang.plugin.json.excel;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ReadExcelJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'readExcel'," +
                "   'titleIndex': 1," +
                "   'path': '/opt/github/fitlang/doc/test/case/自动化测试用例.xls'" +
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

        String output = ExecuteJsonNodeUtil.executeCode("{ 'path': '/opt/github/fitlang/doc/test/case/自动化测试用例.xls'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertNotNull(outputJson.get("list"));

    }

    public void testExecute3() {
        String flow = "{" +//
                "   'uni': 'readExcel'," +
                "   'titleIndex': 0," +
                "   'path': '/opt/github/fitlang/doc/test/case/自动化测试用例.xls'" +
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
                "   'path': '/opt/github/fitlang/doc/test/case/自动化测试用例.xls'" +
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
                "   'path': '/opt/github/fitlang/doc/test/case/自动化测试用例.xls'" +
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
                "            'port': 60003," +
                "            'service': {'/hello':{'uni':'hello'}}," +
                "        }," +
                "        {" +
                "            'flag': '_needFormatJsonInConsole'," +
                "            'uni': 'pipe'," +
                "            'child': [" +
                "                {" +
                "                    'uni': 'mix'," +
                "                    'json': {" +
                "                        'path': '/opt/github/fitlang/doc/test/case/自动化测试用例.xls'," +
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
                "                                    'path': '/opt/github/fitlang/doc/test/case/自动化测试用例.xls'," +
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
}
