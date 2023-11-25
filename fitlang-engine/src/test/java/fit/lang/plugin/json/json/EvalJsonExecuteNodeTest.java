package fit.lang.plugin.json.json;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class EvalJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'eval'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{\"hello\":\"world\"}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"hello\":\"world\"}", output);
    }

    public void testTestExecute1() {

        String flow = "{" +//
                "   'uni': 'eval'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world', 'hello':\"${who}\"}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"who\":\"world\",\"hello\":\"world\"}", output);
    }

}