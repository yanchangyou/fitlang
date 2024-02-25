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
        item.put("timestamp", System.currentTimeMillis());
        item.put("free", 0.5);
        item.put("used", 0.5);
        list.add(item);

        List<String> sumFields = new ArrayList<>();
        sumFields.add("free");
        sumFields.add("used");
        JSONObject result = JsonExecuteNodeMonitorUtil.sumDataInLastSecond(list, 100, sumFields);

        System.out.println(result);

        Assert.assertEquals(2.2, result.get("free"));

        Assert.assertEquals(0.8, result.get("used"));
    }
}