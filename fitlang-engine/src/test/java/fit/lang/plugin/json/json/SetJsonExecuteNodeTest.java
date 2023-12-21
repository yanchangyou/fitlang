package fit.lang.plugin.json.json;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class SetJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +
                "    'uni': 'sequence'," +
                "    'child': [" +
                "       {" +
                "           'uni': 'set'," +
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

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);
        Assert.assertEquals("{\"hello\":\"world\"}", output);

    }
}