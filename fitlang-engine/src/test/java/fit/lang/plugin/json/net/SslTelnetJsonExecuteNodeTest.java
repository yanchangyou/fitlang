package fit.lang.plugin.json.net;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class SslTelnetJsonExecuteNodeTest extends TestCase {

    public void testGetSocket() {
        String flow = "{" +//
                "   'uni': 'telnets'," +
                "   'host': 'www.baidu.com'," +
                "   'port': 443," +
                "   'input': [" +
                "       'GET / HTTP/1.0'," +
//                "       'Host: www.baidu.com'," +
                "       ''," +
                "   ]," +
                "}";
        System.out.println(flow);
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("output"));

    }

    public void testGetSocketByProxy() {
        String flow = "{" +//
                "   'uni': 'telnets'," +
                "   'host': 'www.baidu.com'," +
                "   'port': 443," +
                "   'input': [" +
                "       'GET / HTTP/1.0'," +
                "       'Host: www.baidu.com'," +
                "       ''," +
                "   ]," +
                "   'proxy': {" +
                "        'host': '62.234.182.56'," +
                "        'port': 443" +
                "    }" +
                "}";
        System.out.println(flow.replace("'", "\""));
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("output"));

    }

    public void testGetSocketByProxyWithIP() {
        String flow = "{" +//
                "   'uni': 'telnets'," +
                "   'host': '180.101.50.242'," +
                "   'port': 443," +
                "   'input': [" +
                "       'HEAD / HTTP/1.0'," +
                "       'Host: www.baidu.com'," +
                "       ''," +
                "   ]," +
                "   'proxy': {" +
                "        'host': '62.234.182.56'," +
                "        'port': 443" +
                "    }" +
                "}";
        System.out.println(flow.replace("'", "\""));
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("output"));

    }

    public void testGetSocketByProxyWithIpAndHost() {
        String flow = "{" +//
                "   'uni': 'telnets'," +
                "   'host': '116.196.89.107 www.iteye.com'," +
                "   'port': 443," +
                "   'input': [" +
                "       'HEAD / HTTP/1.0'," +
                "       'Host: www.iteye.com'," +
                "       ''," +
                "   ]," +
                "   'proxy': {" +
                "        'host': '62.234.182.56'," +
                "        'port': 443" +
                "    }" +
                "}";
        System.out.println(flow.replace("'", "\""));
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("output"));

    }
}