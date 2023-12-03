package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSONObject;
import junit.framework.TestCase;

public class ExpressUtilTest extends TestCase {

    public void testEval() {
        JSONObject param = new JSONObject();
        param.put("foo", "bar");
        String express = "${foo}";
        Object result = ExpressUtil.eval(express, param);
        System.out.println(result);
    }
}