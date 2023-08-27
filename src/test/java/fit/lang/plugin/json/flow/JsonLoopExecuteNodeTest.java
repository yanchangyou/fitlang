package fit.lang.plugin.json.flow;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class JsonLoopExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'loop'," +
                "   'isPipe': true," +
                "   'loopTimes': 10," +
                "   'child': {" +
                "       'uni':'mix'," +
                "       'json':{" +
                "           'times':'${times+1}'" +
                "       }" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'times':0}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"times\":10}", output);
    }

    public void testExecute2() {
        String flow = "{" +//
                "   'uni': 'loop'," +
                "   'loopTimes': 10," +
                "   'child': {" +
                "       'uni':'mix'," +
                "       'json':{" +
                "           'times':'${times+1}'" +
                "       }" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'times':0}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"times\":1}", output);
    }

    public void testExecute3() {
        String flow = "{" +//
                "   'uni': 'loop'," +
                "   'loopTimes': 10," +
                "   'child': {" +
                "       'uni':'mix'," +
                "       'json':{" +
                "           'loopIndex':'${loopIndex}'" +
                "       }" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"loopIndex\":9}", output);
    }
}