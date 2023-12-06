package fit.lang.plugin.json.cmd;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class ZipJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        String flow = "{" +//
                "   'uni': 'zip'," +
                "   'path': '/opt/github/fitlang/fitlang-engine/src/test/java/fit/lang/plugin/json/cmd/JavaJsonExecuteNodeTest.java'" +
                "}";

        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("zipPath"));

    }

    public void testExecuteUnzip() {

        String flow = "{" +//
                "   'uni': 'unzip'," +
                "   'path': '/opt/github/fitlang/fitlang-engine/src/test/java/fit/lang/plugin/json/cmd/JavaJsonExecuteNodeTest.java.zip'" +
                "}";

        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("unzipPath"));

    }

    public void testExecuteUnzipDir() {

        String flow = "{" +//
                "   'uni': 'unzip'," +
                "   'path': '/opt/github/fitlang/fitlang-server/demo/fitserver/app/cmd/test.zip'" +
                "}";

        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("unzipPath"));

    }
}