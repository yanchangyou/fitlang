package fit.lang.plugin.json.info;

import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

public class SystemBaseInfoJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {

        String flow = "{'uni':'systemInfo'}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        System.out.println(JSONObject.parse(output).toJSONString(JSONWriter.Feature.PrettyFormat));

        Assert.assertEquals("{\"message\":\"hello, world!\"}", output);

    }

}