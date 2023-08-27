package fit.lang.plugin.json.tool;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;

public class FileServerJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'fileServer'," +
                "   'port': 11101," +
                "   'root': '/opt/log'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
//        Thread.sleep(1000 * 1000);
    }
}