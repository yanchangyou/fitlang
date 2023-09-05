package fit.lang.plugin.json.web;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;

public class WebSocketServerJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {

        String flow = "{" +//
                "   'uni': 'wsServer'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

//        Thread.sleep(1000 * 1000);

    }
}