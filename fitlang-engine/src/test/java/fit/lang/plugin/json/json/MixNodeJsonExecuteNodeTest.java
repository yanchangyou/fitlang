package fit.lang.plugin.json.json;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class MixNodeJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {
        String flow = "{" +//
                "   'uni': 'mixNode'," +
                "   'json':{" +
                "       'info':{'uni':'hello'}" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"info\":{\"message\":\"hello, world!\"}}", output);
    }

}