package fit.lang.plugin.json.flow;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class SequenceJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +
                "    'uni': 'sequence'," +
                "    'isBagsMode': true," +
                "    'bagsName': 'array'," +
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
        Assert.assertEquals("{\"array\":[{\"data\":{\"hello\":\"world\"}},{\"hello\":\"world\"}]}", output);

    }
}