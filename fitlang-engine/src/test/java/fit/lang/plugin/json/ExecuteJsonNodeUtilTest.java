package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSONObject;
import junit.framework.TestCase;
import org.junit.Assert;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.covertToLong;

public class ExecuteJsonNodeUtilTest extends TestCase {

    public void testRemoveJsonComment() {
        String text = null;
        String result = ExecuteJsonNodeUtil.removeJsonComment(text);
        Assert.assertNull(result);
    }

    public void testRemoveJsonComment1() {
        String text = "{}";
        String result = ExecuteJsonNodeUtil.removeJsonComment(text);
        Assert.assertEquals("{}", result);
    }


    public void testRemoveJsonComment2() {
        String text = "{'a':1}";
        String result = ExecuteJsonNodeUtil.removeJsonComment(text);
        Assert.assertEquals("{'a':1}", result);
    }

    public void testRemoveJsonComment3() {
        String text = "{\n" +
                "//comment\n" +
                "'a':1\n" +
                "}";
        String result = ExecuteJsonNodeUtil.removeJsonComment(text);
        Assert.assertEquals("{\n'a':1\n}", result);
    }

    public void testRemoveJsonComment4() {
        String text = "{\r" +
                "//comment\r" +
                "'a':1\r" +
                "}";
        String result = ExecuteJsonNodeUtil.removeJsonComment(text);
        Assert.assertEquals("{\r'a':1\r}", result);
    }

    public void testRemoveJsonComment5() {
        String text = "{\r\n" +
                "//comment\r\n" +
                "'a':1\r\n" +
                "}";
        String result = ExecuteJsonNodeUtil.removeJsonComment(text);
        Assert.assertEquals("{\r\n'a':1\r\n}", result);
    }

    public void testCovertToLong() {
        JSONObject item = new JSONObject();
        item.put("a", 0.99);
        covertToLong(item);
        System.out.println(item);
        Assert.assertEquals(Long.valueOf(1), item.getLong("a"));
    }

    public void test() {
        System.out.println(JSONObject.parse("{1,2}"));
        System.out.println(JSONObject.parse("{01,2}"));
        System.out.println(JSONObject.parse("{1,02}"));
        System.out.println(JSONObject.parse("{01,02}"));
        System.out.println(JSONObject.parse("{10,2}"));
        System.out.println(JSONObject.parse("{1,20}"));
        System.out.println(JSONObject.parse("{100,2}"));
        System.out.println(JSONObject.parse("{1,200}"));
    }
}