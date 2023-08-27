package fit.lang.plugin.json.util;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class MixJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'mix'," +
                "   'json':{" +
                "       'hello':'world'" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"who\":\"world\",\"hello\":\"world\"}", output);
    }

    public void testTestExecute1() {

        String flow = "{" +//
                "   'uni': 'mix'," +
                "   'json':{" +
                "       'hello':\"${who}\"" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"who\":\"world\",\"hello\":\"world\"}", output);
    }
}