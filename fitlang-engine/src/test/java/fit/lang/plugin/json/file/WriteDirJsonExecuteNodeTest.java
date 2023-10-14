package fit.lang.plugin.json.file;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class WriteDirJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'writeDir'," +
                "   'workspaceDir': '/opt/github/fitlang/fitlang-server/demo/fitserver'," +
                "   'dirPath': '/app/dir'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{ 'content': '{\"uni\":\"hello\"}'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("absoluteDir"));

    }
}