package fit.lang.plugin.json.flow;

import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class JsonParallelForeachExecuteNodeTest extends TestCase {

    public void testExecute1() {
        String flow = "{" +//
                "   'uni': 'parallelForeach'," +
                "   'foreachField': 'list'," +
                "   'mixToItemField': 'times'," +
                "   'child': {" +
                "       'uni':'mix'," +
                "       'json':{" +
                "           'message':'mix'" +
                "       }" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'list':['a'],'times':0}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"list\":[{\"data\":\"a\",\"times\":0,\"message\":\"mix\"}]}", output);
    }

    public void testExecute2() {
        String flow = "{" +//
                "   'uni': 'parallelForeach'," +
                "   'parallelism': 16," +
                "   'foreachField': 'list'," +
                "   'mixToItemField': 'times'," +
                "   'child': {" +
                "       'uni':'mix'," +
                "       'json':{" +
                "           'message':'mix'" +
                "       }" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'list':['a'],'times':0}", flow);

        System.out.println(output);

        Assert.assertEquals("{\"list\":[{\"data\":\"a\",\"times\":0,\"message\":\"mix\"}]}", output);
    }

}