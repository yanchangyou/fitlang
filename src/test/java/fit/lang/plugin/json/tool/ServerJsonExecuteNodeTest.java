package fit.lang.plugin.json.tool;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;

public class ServerJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'server'," +
                "   'port': 11110," +
                "   'root': '/opt/log'," +
                "   'action':{" +
                "       '/hello':{" +
                "           'uni':'hello'" +
                "       }" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
//        Thread.sleep(1000 * 1000);
    }
}