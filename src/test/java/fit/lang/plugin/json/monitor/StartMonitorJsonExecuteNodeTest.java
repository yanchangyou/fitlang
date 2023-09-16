package fit.lang.plugin.json.monitor;

import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;

import java.util.List;

public class StartMonitorJsonExecuteNodeTest extends TestCase {

    public void testExecute() throws InterruptedException {

        String flow = "{'uni':'monitor','second':1}";

        ExecuteJsonNodeUtil.executeCode(flow);

        Thread.sleep(3 * 1000);

        List<JSONObject> cupGathers = StartMonitorJsonExecuteNode.getCpuGatherList(10);

        System.out.println(cupGathers);

    }
}