package fit.lang.plugin.json.info;

import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

public class InfoJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {

        String flow = "{'uni':'info','shareFields':['env']}";

        String output = ExecuteJsonNodeUtil.executeCode("{'who':'world'}", flow);

        JSONObject info = JSONObject.parse(output);
        System.out.println(toJsonTextWithFormat(info));

        Assert.assertTrue(info.containsKey("env"));

    }

}