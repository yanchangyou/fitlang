package fit.lang.plugin.json.monitor;

import com.alibaba.fastjson2.JSONObject;
import junit.framework.TestCase;
import org.junit.Assert;

import java.util.ArrayList;
import java.util.List;

public class JsonExecuteNodeMonitorUtilTest extends TestCase {

    public void testSumDataInLastSecond() {
        List<JSONObject> list = new ArrayList<>();

        JSONObject item = new JSONObject();
        item.put("timestamp", System.currentTimeMillis() - 2000);
        item.put("free", 0.9);
        item.put("used", 0.1);
        list.add(item);

        item = new JSONObject();
        item.put("timestamp", System.currentTimeMillis() - 1000);
        item.put("free", 0.8);
        item.put("used", 0.2);
        list.add(item);

        item = new JSONObject();
        item.put("timestamp", System.currentTimeMillis() - 0);
        item.put("free", 0.5);
        item.put("used", 0.5);
        list.add(item);

        JSONObject result = JsonExecuteNodeMonitorUtil.sumDataInLastSecond(list, 100);

        System.out.println(result);

        Assert.assertEquals(1300.0, result.get("free"));

        Assert.assertEquals(700.0, result.get("used"));
    }
}