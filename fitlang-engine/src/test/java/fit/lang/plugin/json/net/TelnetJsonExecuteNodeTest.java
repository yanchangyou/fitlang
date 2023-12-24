package fit.lang.plugin.json.net;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class TelnetJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'telnet'," +
                "   'host': 'fit.321zou.com'," +
                "   'port': 80," +
                "   'input': [" +
                "       'GET /hello HTTP/1.0'," +
                "       'Host: fit.321zou.com'," +
                "       ''," +
                "   ]," +
                "}";
        System.out.println(flow);
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("output"));

    }

    public void testExecuteWithProxy() {
        String flow = "{" +//
                "   'uni': 'telnet'," +
                "   'host': 'fit.321zou.com'," +
                "   'port': 80," +
                "   'input': [" +
                "       'GET /hello HTTP/1.0'," +
                "       'Host: fit.321zou.com'," +
                "       ''," +
                "   ]," +
                "   'proxy': {" +
                "        'host': '62.234.182.56'," +
                "        'port': 443" +
                "    }" +
                "}";
        System.out.println(flow);
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("output"));

    }
}