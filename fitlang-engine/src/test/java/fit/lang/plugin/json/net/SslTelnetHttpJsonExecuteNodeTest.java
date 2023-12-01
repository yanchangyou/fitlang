package fit.lang.plugin.json.net;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class SslTelnetHttpJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'telnet.https'," +
                "   'method': 'HEAD'," +
                "   'url': 'https://180.101.50.242'," +
                "    'header': {" +
                "        'Host': 'www.baidu.com'" +
                "    }," +
                "}";
        System.out.println(flow.replace("'", "\""));
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("output"));

    }
}