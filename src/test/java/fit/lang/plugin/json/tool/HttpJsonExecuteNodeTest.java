package fit.lang.plugin.json.tool;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class HttpJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        String flow = "{" +//
                "   'uni':'http'," +
                "   'url':'https://aip.baidubce.com/rest/2.0/ocr/v1/general_basic'," +
                "   'header':{" +
                "       'test':'123'" +
                "   }" +
                "}";
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertTrue(!output.isEmpty());

        Assert.assertEquals("{'error_code':100,'error_msg':'Invalid parameter'}", output);

    }


    public void testExecute2() {

        String flow = "{" +
                "    'uni': 'sequence'," +
                "    'child': [" +
                "        {" +
                "            'uni': 'server'," +
                "            'port': 60000," +
                "            'service': {" +
                "                '/echo': {" +
                "                    'uni': 'echo'" +
                "                }" +
                "            }" +
                "        }," +
                "        {" +
                "            'uni': 'http'," +
                "            'param': {" +
                "                'hello': 'world'" +
                "            }," +
                "            'url': 'http://127.0.0.1:60000/echo'" +
                "        }" +
                "    ]" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertTrue(!output.isEmpty());

        Assert.assertEquals("{'hello':'world'}", output);

    }


    public void testExecute3() {

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