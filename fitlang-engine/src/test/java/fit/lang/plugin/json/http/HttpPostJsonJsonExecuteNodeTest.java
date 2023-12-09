package fit.lang.plugin.json.http;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class HttpPostJsonJsonExecuteNodeTest extends TestCase {

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

        Assert.assertEquals("{\"error_code\":100,\"error_msg\":\"Invalid parameter\"}", output);

    }

    public void testExecuteWithProxy() {

        String flow = "{" +//
                "   'uni':'httpGet'," +
                "   'url':'https://www.baidu.com'," +
                "   'header':{" +
//                "       'Host':'www.baidu.com'" +
                "   }," +
                "    'proxy': {" +
                "        'host': '62.234.182.56'," +
                "        'port': 443" +
                "    }" +
                "}";
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertTrue(!output.isEmpty());
        Assert.assertTrue(output.toString().contains("百度"));


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

}