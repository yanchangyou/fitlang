package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class SleepJsonExecuteNodeTest extends TestCase {

    public void testExecute1() {
        String flow = "{" +//
                "   'uni': 'sleep'," +
                "   'second': 0.1" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);
    }

    public void testExecute2() {
        String flow = "{" +//
                "   'uni': 'sleep'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'second': 0.1}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);
    }

    public void testExecute3() {
        String flow = "{" +//
                "   'uni': 'sleep'," +
                "   'second': '${second+0.1}'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'second': 0.1}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);
    }
}