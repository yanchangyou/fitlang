package fit.lang.plugin.json.flow;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class BatchJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +
                "    'uni': 'batch'," +
                "    'child': [" +
                "       {" +
                "           'uni': 'setGlobal'," +
                "           'key': 'data'," +
                "           'value':{" +
                "               'hello':'world'" +
                "           }" +
                "        }," +
                "        {" +
                "            'uni': 'mix'," +
                "            'json': {" +
                "               'hello':\"${data.hello}\"" +
                "             }" +//
                "        }" +//
                "     ]" +
                "}";
        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"list\":[{},{\"hello\":\"world\"}]}", output);

    }
}