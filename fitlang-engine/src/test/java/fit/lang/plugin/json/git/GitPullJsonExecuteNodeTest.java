package fit.lang.plugin.json.git;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class GitPullJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'gitPull'," +
                "   'gitPath': '.'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("message"));

    }
}