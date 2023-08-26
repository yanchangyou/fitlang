package fit.lang.plugin.json.util;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.define.base.ExecuteNode;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteContext;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import junit.framework.TestCase;
import org.junit.Assert;

public class HelloJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        JSONObject output = ExecuteJsonNodeUtil.execute(JSON.parseObject("{'who':'world'}"), new HelloJsonExecuteNode());

        Assert.assertTrue(!output.isEmpty());

        System.out.println(output);

        Assert.assertTrue(output.containsKey("message"));

        Assert.assertEquals("hello, world!", output.getString("message"));

    }
}