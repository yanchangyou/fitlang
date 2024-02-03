package fit.lang.plugin.json.file;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import junit.framework.TestCase;
import org.junit.Assert;

public class ReadFileJsonExecuteNodeTest extends TestCase {

    @Override
    protected void setUp() throws Exception {
        JsonDynamicFlowExecuteEngine.enableUnsafeNodes();
    }

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'readFile'," +
                "   'workspaceDir': '/opt/github/fitlang/fitlang-server/demo/fitserver'," +
                "   'filePath': '/app/first.fit'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("message"));

        Assert.assertEquals("{\"uni\":\"hello\",\"message\":\"hello, world! 你好，世界！\",\"absolutePath\":\"/opt/github/fitlang/fitlang-server/demo/fitserver/app/first.fit\"}", outputJson.toJSONString());

    }

    public void testExecuteDir() {
        String flow = "{" +//
                "   'uni': 'readFile'," +
                "   'workspaceDir': '/opt/github/fitlang/fitlang-server/demo/fitserver'," +
                "   'filePath': '/app/'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("files"));
        Assert.assertFalse(outputJson.getBoolean("isFile"));

        Assert.assertTrue(outputJson.getJSONArray("files").getJSONObject(0).containsKey("name"));

//        HexUtil.encodeHexStr();
    }
}