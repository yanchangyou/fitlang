package fit.lang.plugin.json.json;

import com.alibaba.fastjson2.JSONObject;
import junit.framework.TestCase;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.sortJson;

public class SortFieldJsonExecuteNodeTest extends TestCase {

    public void testExecute() {

        JSONObject json = JSONObject.parseObject("{'d':[{'d2':2,'d1':1},{'d1':11,'d2':22}],'c':{'c2':2,'c1':1},'b':2,'a':1}");
        System.out.println(json);

        JSONObject out = sortJson(json);
        System.out.println(out);
    }

}