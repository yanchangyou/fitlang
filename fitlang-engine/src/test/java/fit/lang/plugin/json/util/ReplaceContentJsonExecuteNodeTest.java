package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ReplaceContentJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        String flow = "{" +//
                "   'uni': 'replaceContent'," +
                "   'find': '[a-z]+'," +
                "   'replace': '123'," +
                "   'content': 'abc456'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("content"));

        Assert.assertEquals("123456", outputJson.getString("content"));

    }
}