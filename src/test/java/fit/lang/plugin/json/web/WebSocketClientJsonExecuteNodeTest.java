package fit.lang.plugin.json.web;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;

public class WebSocketClientJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        String flow = "{" +//
                "   'uni': 'wsClient'," +
                "   'url': 'ws://127.0.0.1:10000/ws'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

    }
}