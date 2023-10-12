package fit.lang.plugin.json.file;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class WriteFileJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'writeFile'," +
                "   'workspaceDir': '/opt/github/fitlang/fitlang-server/demo/fitserver'," +
                "   'path': '/app/test.fit'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{ 'content': '{\"uni\":\"hello\"}'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("path"));

        Assert.assertEquals("{\n" +
                "    \"uni\": \"hello\",\n" +
                "    \"message\": \"hello, world!\"\n" +
                "}", outputJson.getString("content"));

    }
}