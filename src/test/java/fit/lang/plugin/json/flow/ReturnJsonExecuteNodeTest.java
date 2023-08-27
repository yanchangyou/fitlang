package fit.lang.plugin.json.flow;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ReturnJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'return'," +
                "   'json':{" +
                "       'hello':'world'" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"hello\":\"world\"}", output);
    }

    public void testTestExecute1() {

        String flow = "{" +//
                "   'uni': 'return'," +
                "   'json':{" +
                "       'hello':\"${who}\"" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"hello\":\"world\"}", output);
    }

    public void testTestExecute2() {

        String flow = "{" +//
                "   'uni': 'return'," +
                "   'returnField': 'hello'," +
                "   'json':{" +
                "       'hello':\"${who}\"" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        System.out.println(output);
        Assert.assertEquals("world", output);
    }
}