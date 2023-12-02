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

    public void testExecuteWithOption() {
        String flow = "{" +//
                "   'uni': 'cmd'," +
                "   'cmd': 'ls ${option}'" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{'option':' -l'}", flow);

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
        JSONObject option = new JSONObject();
        CmdJsonExecuteNode cmdJsonExecuteNode = new CmdJsonExecuteNode();
        option.put("n", 1);
        for (String cmd : cmdArray) {
            String result = cmdJsonExecuteNode.parseCmd(cmd, option);
            System.out.println(result);
        }
    }

    public void testExecuteWithOption1() {
        String flow = "{" +//
                "   'uni': 'cmd'," +
                "   'cmd': 'ping www.baidu.com'," +
                "   'option': {" +
                "       '-c': '2'," +
                "   }" +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("result"));

    }

    public void testExecuteWithOption2() {
        String flow = "{" +//
                "   'uni': 'cmd:java'," +
                "   'option': {" +
                "       '-XX:': '+PrintGCDetails'," +
                "    }," +
                "    'target':'-version'," +
                "}";

        String output = ExecuteJsonNodeUtil.executeCode("{}", flow);

        JSONObject outputJson = JSON.parseObject(output);

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(outputJson.containsKey("result"));

    }
}