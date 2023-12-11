package fit.lang.plugin.json.cmd;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class GoJsonExecuteNodeTest extends TestCase {

    public void testExecuteWithParam2() {
        String flow = "{" +//
                "   'uni': 'cmd:go'," +
                "   'target': 'version'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("result"));

    }
}