package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class CatchJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'catch'," +
                "   'child': {" +
                "       'uni':'mix'," +
                "       'json':{" +
                "           'message':'${1/0}'" +
                "       }" +
                "   }" +
                "}";

        JSONObject output = ExecuteJsonNodeUtil.execute(JSON.parseObject(flow));

        System.out.println(output);

        Assert.assertTrue(output.containsKey("exception"));
    }
}