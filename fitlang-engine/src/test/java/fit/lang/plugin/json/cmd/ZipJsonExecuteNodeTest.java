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
                "   'filePath': '/opt/github/fitlang/fitlang-engine/src/test/java/fit/lang/plugin/json/cmd/ZipJsonExecuteNodeTest.java'," +
                "   'filePrefix': 'ZipJsonExecuteNodeTest'" +
                "}";

        System.out.println(flow.replace("'", "\""));

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("zipPath"));


    }
}