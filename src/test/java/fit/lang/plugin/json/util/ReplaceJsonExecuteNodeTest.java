package fit.lang.plugin.json.util;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ReplaceJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'replace'," +
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
                "   'uni': 'replace'," +
                "   'json':{" +
                "       'hello':\"${who}\"" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"hello\":\"world\"}", output);
    }
}