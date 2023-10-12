package fit.lang.plugin.json.cloud;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class CloudServerJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        String flow = "{'uni':'cloudServer','port':20000}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"port\":20000,\"message\":\"start web socket at port: 20000\"}", output);

    }
}