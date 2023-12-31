package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class PerformanceJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'perf'," +
                "   'child': {" +
                "       'uni':'mix'," +
                "       'json':{" +
                "           'message':'mix'" +
                "       }" +
                "   }" +
                "}";

        JSONObject output = ExecuteJsonNodeUtil.execute(JSON.parseObject(flow));

        System.out.println(output);

        Assert.assertEquals("{\"message\":\"mix\"}", output.getString("output"));
        Assert.assertTrue(output.containsKey("executeInfo"));
        Assert.assertTrue(output.getJSONObject("executeInfo").containsKey("beginTime"));
        Assert.assertTrue(output.getJSONObject("executeInfo").containsKey("costTime"));
        Assert.assertTrue(output.getJSONObject("executeInfo").containsKey("endTime"));
    }

    public void testExecute2() {

        String flow = "{" +//
                "   'uni': 'perf'," +
                "   'child': {" +
                "       'uni': 'loop'," +
                "       'isPipe': true," +
                "       'loopTimes': 1000," +
                "       'child': {" +
                "           'uni':'mix'," +
                "           'json':{" +
                "               'times':'${loopIndex+1}'," +
                "               'times1':'${loopIndex+1}'" +
                "           }" +
                "       }" +
                "   }" +
                "}";

        System.out.println(flow);

        JSONObject output = ExecuteJsonNodeUtil.execute(JSON.parseObject(flow));

        System.out.println(output);

        Assert.assertTrue(output.containsKey("executeInfo"));
        Assert.assertTrue(output.getJSONObject("executeInfo").containsKey("beginTime"));
        Assert.assertTrue(output.getJSONObject("executeInfo").containsKey("costTime"));
        Assert.assertTrue(output.getJSONObject("executeInfo").containsKey("endTime"));
    }
}