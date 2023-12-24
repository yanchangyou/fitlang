package fit.lang.plugin.json.json;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class GetJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +
                "    'uni': 'sequence'," +
                "    'child': [" +
                "       {" +
                "           'uni': 'setGlobal'," +
                "           'key': 'data'," +
                "           'value':{" +
                "               'hello':'world'" +
                "           }" +
                "        }," +
                "        {" +
                "            'uni': 'get'," +
                "            'path': 'data'" +//
                "        }" +//
                "     ]" +
                "}";
        System.out.println(flow.replace("'", "\""));
        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"hello\":\"world\"}", output);

    }
}