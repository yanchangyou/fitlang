package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import junit.framework.TestCase;
import org.junit.Assert;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.*;

public class SortFieldJsonExecuteNodeTest extends TestCase {

    public void testSortJson() {

        JSONObject json = JSONObject.parseObject("{'f':'abc','e':null, 'd':[{'d2':2,'d1':1},{'d1':11,'d2':22}],'c':{'c2':2,'c1':1},'b':2,'a':1}");
        System.out.println(json);

        JSONObject out = ExecuteJsonNodeUtil.sortJsonField(json);
        System.out.println(toJsonText(out));
        Assert.assertEquals("{\"a\":1,\"b\":2,\"c\":{\"c1\":1,\"c2\":2},\"d\":[{\"d1\":1,\"d2\":2},{\"d1\":11,\"d2\":22}],\"e\":null,\"f\":\"abc\"}", toJsonText(out));
    }

    public void testGetStruct() {

        JSONObject json = JSONObject.parseObject("{'f':'abc','e':null, 'd':[{'d2':2,'d1':1},{'d1':11,'d2':22}],'c':{'c2':2,'c1':1},'b':2,'a':1}");
        System.out.println(json);

        JSONObject out = getJsonStruct(json);
        System.out.println(toJsonText(out));
        Assert.assertEquals("{\"a\":0,\"b\":0,\"c\":{\"c1\":0,\"c2\":0},\"d\":[{\"d1\":0,\"d2\":0}],\"e\":null,\"f\":\"\"}", toJsonText(out));

    }
}