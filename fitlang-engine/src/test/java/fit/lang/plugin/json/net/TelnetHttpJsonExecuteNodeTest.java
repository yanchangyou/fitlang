package fit.lang.plugin.json.net;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class TelnetHttpJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'telnet.http'," +
                "   'method': 'GET'," +
                "   'url': 'http://116.62.65.251/hello'," +
                "    'header': {" +
                "        'Host': 'fit.321zou.com'" +
                "    }," +
                "}";
        System.out.println(flow);
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("output"));

    }

    public void testExecute2() {
        String flow = "{" +//
                "   'uni': 'telnet.http'," +
                "   'method': 'GET'," +
                "   'url': 'http://116.62.65.251/404'," +
                "    'header': {" +
                "        'Host': 'fit.321zou.com'" +
                "    }," +
                "}";
        System.out.println(flow);
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("output"));

    }
}