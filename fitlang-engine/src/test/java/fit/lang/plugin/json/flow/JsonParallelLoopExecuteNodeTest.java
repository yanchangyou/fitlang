package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class JsonParallelLoopExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'loop'," +
                "   'isPipe': true," +
                "   'parallelism': 6," +
                "   'loopTimes': 100," +
                "   'child': {" +
                "       'uni':'mix'," +
                "       'json':{" +
                "           'times':'${times+1}'" +
                "       }" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'times':0}", flow);

        System.out.println(output);

//        Assert.assertEquals("{\"times\":10}", output);
    }

    public void testExecute2() {
        String flow = "{" +//
                "   'uni': 'loop'," +
                "   'isPipe': true," +
                "   'isBagsMode': true," +
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

        Assert.assertEquals(10, JSONObject.parseObject(output).getJSONArray("list").size());

    }

    public void testExecuteHttp() {
        int loopTimes = 10;
        String flow = "{" +//
                "   'uni': 'loop'," +
                "   'isPipe': true," +
                "   'isBagsMode': true," +
                "   'parallelism': 8," +
                "   'loopTimes': " + loopTimes + "," +
                "   'child': {" +
                "       'uni':'http'," +
                "       'url':'http://fit.321zou.com/hello'" +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'times':0}", flow);

        System.out.println(output);

        Assert.assertEquals(loopTimes, JSONObject.parseObject(output).getJSONArray("list").size());

    }
}