package fit.lang.plugin.json.cloud;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class CloudServerJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        String flow = "{'uni':'register','registerUrl':'http://127.0.0.1:11111/_register'}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"message\":\"hello, world!\"}", output);

    }

}