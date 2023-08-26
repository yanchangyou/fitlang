package fit.lang.plugin.json.flow;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ReturnJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'return'," +
                "   'value':{" +
                "       'hello':'world'" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"hello\":\"world\"}", output);
    }
}