package fit.lang.plugin.json.http;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class HttpJsonExecuteNodeTest extends TestCase {

    public void testPostMan() {
        String flow = "{" +
                "    'uni': 'postman'," +
                "    'method': 'POST'," +
                "    'url': 'http://fit.321zou.com/echo'," +
                "    'header': {" +
                "        'contentType': 'application/json'" +
                "    }," +
                "    'query': {" +
                "        'foo': 'bar'" +
                "    }," +
                "    'body': {" +
                "        'hello': 'world'" +
                "    }," +
                "    'proxy': {" +
                "        'host': ''," +
                "        'port': 0" +
                "    }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertTrue(output.contains("{\"hello\":\"world\",\"foo\":\"bar\"}"));
        Assert.assertTrue(output.contains("\"cookie\""));
        Assert.assertTrue(output.contains("\"header\""));
        Assert.assertTrue(output.contains("\"status\""));
        Assert.assertTrue(output.contains("\"size\""));
        Assert.assertTrue(output.contains("\"time\""));
        Assert.assertTrue(output.contains("\"body\""));
    }

    public void testHttpNotOnlyBody() {
        String flow = "{" +
                "    'uni': 'http'," +
                "    'onlyBody': false," +
                "    'method': 'POST'," +
                "    'url': 'http://fit.321zou.com/echo'," +
                "    'header': {" +
                "        'contentType': 'application/json'" +
                "    }," +
                "    'query': {" +
                "        'foo': 'bar'" +
                "    }," +
                "    'body': {" +
                "        'hello': 'world'" +
                "    }," +
                "    'proxy': {" +
                "        'host': ''," +
                "        'port': 0" +
                "    }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertTrue(output.contains("{\"hello\":\"world\",\"foo\":\"bar\"}"));
        Assert.assertTrue(output.contains("\"cookie\""));
        Assert.assertTrue(output.contains("\"header\""));
        Assert.assertTrue(output.contains("\"status\""));
        Assert.assertTrue(output.contains("\"size\""));
        Assert.assertTrue(output.contains("\"time\""));
        Assert.assertTrue(output.contains("\"body\""));
    }

    public void testHttp() {
        String flow = "{" +
                "    'uni': 'http'," +
                "    'method': 'POST'," +
                "    'url': 'http://fit.321zou.com/echo'," +
                "    'header': {" +
                "        'contentType': 'application/json'" +
                "    }," +
                "    'query': {" +
                "        'foo': 'bar'" +
                "    }," +
                "    'body': {" +
                "        'hello': 'world'" +
                "    }," +
                "    'proxy': {" +
                "        'host': ''," +
                "        'port': 0" +
                "    }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"hello\":\"world\",\"foo\":\"bar\"}", output);
    }

    public void testUniUrl() {
        String flow = "{" +
                "    'uni': 'POST http://fit.321zou.com/hello'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertTrue(output.contains("{\"message\":\"hello, world!\"}"));
    }

    public void testExecuteHead() {
        String flow = "{" +//
                "   'uni': 'httpHead'," +
                "   'url': 'http://fit.321zou.com/hello'," +
                "}";
        System.out.println(flow.replace("'", "\""));
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertEquals("", outputJson.get("_raw"));

    }

    public void testExecuteHead2() {
        String flow = "{" +//
                "   'uni': 'httpHead'," +
                "   'url': 'https://aip.baidubce.com/rest/2.0/ocr/v1/general_basic'," +
                "}";
        System.out.println(flow.replace("'", "\""));
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertEquals("", outputJson.get("_raw"));

    }

}