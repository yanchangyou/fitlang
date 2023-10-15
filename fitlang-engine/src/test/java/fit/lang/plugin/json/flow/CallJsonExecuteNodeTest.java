package fit.lang.plugin.json.flow;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class CallJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'switch'," +
                "   'switchField': 'type'," +
                "   'child': [" +
                "       {" +
                "           'id':'case2'," +
                "           'case':'2'," +
                "           'uni':'echo'" +
                "       }," +
                "       {" +
                "           'case':'1'," +
                "           'uni':'call'," +
                "           'nodeId':'case2'," +
                "       }," +
                "   ]" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'type':'1'}", flow);

        System.out.println(output);

        JSONObject result = JSONObject.parse(output);
        Assert.assertEquals("1", result.get("type"));
    }
}