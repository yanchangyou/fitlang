package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class DecreaseJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'decrease'," +
                "   'field': 'num'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'num':0}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("num"));
        Assert.assertEquals(-1, outputJson.get("num"));

    }
}