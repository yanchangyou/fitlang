package fit.lang.plugin.json.tool;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class ProxyJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'proxy'," +
                "   'url': 'http://127.0.0.1:11101'" +
                "}";
        JSONObject contextParam = new JSONObject();
        contextParam.put(ServerJsonExecuteNode.REQUEST_PATH, "/index.html");
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow, contextParam);

        System.out.println(output);

        Assert.assertEquals("{\"_raw\":\"hello,world!\\n\"}", output);
    }

    public void testExecute2() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'proxy'," +
                "   'url': 'http://127.0.0.1:11101/'" +
                "}";
        JSONObject contextParam = new JSONObject();
        contextParam.put(ServerJsonExecuteNode.REQUEST_PATH, "/static/sub/page.html");
        contextParam.put(ServerJsonExecuteNode.SERVICE_PATH, "/static");
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow, contextParam);

        System.out.println(output);

        Assert.assertEquals("{\"_raw\":\"hello,sub!\\n\"}", output);
    }

    public void testExecute3() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'proxy'," +
                "   'url': 'http://127.0.0.1:11101/'" +
                "}";
        JSONObject contextParam = new JSONObject();
        contextParam.put(ServerJsonExecuteNode.REQUEST_PATH, "/static/sub/page.html");
        contextParam.put(ServerJsonExecuteNode.SERVICE_PATH, "/static/");
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow, contextParam);

        System.out.println(output);

        Assert.assertEquals("{\"_raw\":\"hello,sub!\\n\"}", output);
    }
}