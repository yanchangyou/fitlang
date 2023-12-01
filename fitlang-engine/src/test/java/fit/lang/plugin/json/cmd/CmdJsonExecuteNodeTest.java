package fit.lang.plugin.json.cmd;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class CmdJsonExecuteNodeTest extends TestCase {

    public void testExecute() {
        String flow = "{" +//
                "   'uni': 'cmd'," +
                "   'cmd': 'ls'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("result"));

    }

    public void testExecuteWithParam() {
        String flow = "{" +//
                "   'uni': 'cmd'," +
                "   'cmd': 'ls ${param}'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'param':' -l'}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("result"));

    }
    public void testParseCmd() {
        String[] cmdArray = new String[]{
                "ls -n ${n}",
                "ls -n ${n1}",
                "ls -n ${n} -c ${n}",
        };
        JSONObject param = new JSONObject();
        CmdJsonExecuteNode cmdJsonExecuteNode = new CmdJsonExecuteNode();
        param.put("n", 1);
        for (String cmd : cmdArray) {
            String result = cmdJsonExecuteNode.parseCmd(cmd, param);
            System.out.println(result);
        }
    }

    public void testExecuteWithParam1() {
        String flow = "{" +//
                "   'uni': 'cmd'," +
                "   'cmd': 'ping www.baidu.com'," +
                "   'param': {" +
                "       '-c': '2'," +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("result"));

    }

    public void testExecuteWithParam2() {
        String flow = "{" +//
                "   'uni': 'cmd'," +
                "   'cmd': 'java'," +
                "   'param': {" +
                "       '-version': ''," +
                "       '-XX:': '+PrintGCDetails'," +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("result"));

    }
}