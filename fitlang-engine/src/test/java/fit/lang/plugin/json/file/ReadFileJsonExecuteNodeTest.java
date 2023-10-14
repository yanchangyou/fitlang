package fit.lang.plugin.json.file;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ReadFileJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'readFile'," +
                "   'workspaceDir': '/opt/github/fitlang/fitlang-server/demo/fitserver'," +
                "   'filePath': '/app/first.fit'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("content"));

        Assert.assertEquals("{\n" +
                "    \"uni\": \"hello\",\n" +
                "    \"message\": \"hello, world!\"\n" +
                "}", outputJson.getString("content"));

    }

    public void testExecuteDir() {
        String flow = "{" +//
                "   'uni': 'readFile'," +
                "   'workspaceDir': '/opt/github/fitlang/fitlang-server/demo/fitserver'," +
                "   'filePath': '/app/'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("files"));
        Assert.assertFalse(outputJson.getBoolean("isFile"));

        Assert.assertTrue(outputJson.getJSONArray("files").getJSONObject(0).containsKey("name"));

    }
}