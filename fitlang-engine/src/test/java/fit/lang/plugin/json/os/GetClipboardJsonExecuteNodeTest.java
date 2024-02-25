package fit.lang.plugin.json.os;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.util.HelloJsonExecuteNode;
import junit.framework.TestCase;
import org.junit.Assert;

public class GetClipboardJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        HelloJsonExecuteNode helloExecuteNode = new HelloJsonExecuteNode();
        helloExecuteNode.setNodeDefine(JSONObject.parseObject("{'uni':'getClipboard'}"));

        JSONObject output = ExecuteJsonNodeUtil.execute(JSON.parseObject("{}"), helloExecuteNode);

        Assert.assertFalse(output.isEmpty());

    }

}