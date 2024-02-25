package fit.lang.plugin.json.file;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.JsonDynamicFlowExecuteEngine;
import junit.framework.TestCase;
import org.junit.Assert;

public class DeleteFileJsonExecuteNodeTest extends TestCase {

    @Override
    protected void setUp() {
        JsonDynamicFlowExecuteEngine.enableUnsafeNodes();
    }

    public void testExecute() {
        String flow = "{" +
                "   'uni':'pipe'," +
                "   'child':[" +
                "       {" +//
                "           'uni': 'writeFile'," +
                "           'workspaceDir': '/opt/github/fitlang/fitlang-server/demo/fitserver'," +
                "           'filePath': '/app/new.fit'," +
                "       }," +
                "       {" +//
                "           'uni': 'deleteFile'," +
                "           'workspaceDir': '/opt/github/fitlang/fitlang-server/demo/fitserver'," +
                "           'filePath': '/app/new.fit'," +
                "       }" +
                "   ]," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{ 'content': '{\"uni\":\"hello\"}'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertFalse(output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("absolutePath"));

    }
}