package fit.lang.plugin.json.tool;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;


public class ServerJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        ServerJsonExecuteNode node = new ServerJsonExecuteNode();
        String flow = "{" +//
                "   'port': 11111," +
                "   'header':{" +
                "       'test':'123'" +
                "   }" +
                "}";
        node.setNodeDefine(JSON.parseObject(flow));

        JSONObject output = ExecuteJsonNodeUtil.execute(JSON.parseObject("{}"), node);

        System.out.println(output);
        Thread.sleep(1000 * 1000);
    }
}