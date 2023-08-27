package fit.lang.plugin.json.tool;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;

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
    }
}