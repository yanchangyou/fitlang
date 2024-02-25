package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
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

    public void testDiffJsonObject() {
        JSONObject json1 = JSONObject.parseObject("{" +
                "'number':1," +
                "'string':'abc'," +
                "'object': {'number':1,'string':'abc',}," +
                "'array': [{'number':1,'string':'abc',}]," +
                "}");
        JSONObject json2 = JSONObject.parseObject("{" +
                "'number':1," +
                "'string':'abc'," +
                "'object': {'number':1,'string':'abc',}," +
                "'array': [{'number':1,'string':'abc',}]," +
                "}");
        JSONObject result = ExecuteJsonNodeUtil.diffJsonObject(json1, json2);
        System.out.println(result.toJSONString(JSONWriter.Feature.PrettyFormat));
        String expected = "{}";
        Assert.assertEquals(expected, result.toString());
    }

    public void testDiffJsonObject1() {
        JSONObject json1 = JSONObject.parseObject("{" +
                "'number':1," +
                "'string':'abc'," +
                "'object': {'number':1,'string':'abc',}," +
                "'array': [{'number':1,'string':'abc',}]," +
                "}");
        JSONObject json2 = JSONObject.parseObject("{" +
                "'number':1," +
                "'string':'abc'," +
                "'object': {'number':1,'string':'abc',}," +
                "'array': [{'number':1,'string':'abc',}]," +
                "}");
        JSONObject result = ExecuteJsonNodeUtil.diffJsonObject(json1, json2);
        System.out.println(result.toJSONString(JSONWriter.Feature.PrettyFormat));
        String expected = "{}";
        Assert.assertEquals(expected, result.toString());
    }

    public void testDiffJsonObject2() {
        JSONObject json1 = JSONObject.parseObject("{" +
                "'number':'abc'," +
                "'string':123," +
                "'object': {'number':'abc','string':123,}," +
                "'array': [{'number':'abc','string':123,}]," +
                "}");
        JSONObject json2 = JSONObject.parseObject("{" +
                "'number':1," +
                "'string':'abc'," +
                "'object': {'number':123,'string':'abc',}," +
                "'array': [{'number':123,'string':'abc',}]," +
                "}");
        JSONObject result = ExecuteJsonNodeUtil.diffJsonObject(json1, json2);
        System.out.println(result.toJSONString(JSONWriter.Feature.PrettyFormat));
        String expected = "{\"object.string\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"number\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"string\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"array[0].number\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"array[0].string\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"object.number\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"}}";
        Assert.assertEquals(expected, result.toString());
    }

    public void testDiffJsonObject3() {
        JSONObject json1 = JSONObject.parseObject("{" +
//                "'number':'abc'," +
                "'string':123," +
                "'object': {'number':'abc','string':123,}," +
                "'array': [{'number':'abc','string':123,}]," +
                "}");
        JSONObject json2 = JSONObject.parseObject("{" +
                "'number':1," +
//                "'string':'abc'," +
//                "'object': {'number':123,'string':'abc',}," +
                "'array': [{'number':123,'string':'abc',}]," +
                "}");
        JSONObject result = ExecuteJsonNodeUtil.diffJsonObject(json1, json2);
        System.out.println(result.toJSONString(JSONWriter.Feature.PrettyFormat));
        String expected = "{\"object.string\":{\"equal\":false,\"type\":\"REMOVE\"},\"number\":{\"equal\":false,\"type\":\"ADD\"},\"string\":{\"equal\":false,\"type\":\"REMOVE\"},\"array[0].number\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"array[0].string\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"object.number\":{\"equal\":false,\"type\":\"REMOVE\"}}";
        Assert.assertEquals(expected, result.toString());
    }

    public void testCompareJsonObject() {
        JSONObject json1 = JSONObject.parseObject("{" +
                "'number':1," +
                "'string':'abc'," +
                "'object': {'number':1,'string':'abc',}," +
                "'array': [{'number':1,'string':'abc',}]," +
                "}");
        JSONObject json2 = JSONObject.parseObject("{" +
                "'number':1," +
                "'string':'abc'," +
                "'object': {'number':1,'string':'abc',}," +
                "'array': [{'number':1,'string':'abc',}]," +
                "}");
        JSONObject result = ExecuteJsonNodeUtil.compareJsonObject(json1, json2);
        System.out.println(result.toJSONString(JSONWriter.Feature.PrettyFormat));
        String expected = "{\"object.string\":{\"equal\":true},\"number\":{\"equal\":true},\"string\":{\"equal\":true},\"array[0].number\":{\"equal\":true},\"array[0].string\":{\"equal\":true},\"object.number\":{\"equal\":true}}";
        Assert.assertEquals(expected, result.toString());
    }

    public void testCompareJsonObject1() {
        JSONObject json1 = JSONObject.parseObject("{" +
                "'number':11," +
                "'string':'abcd'," +
                "'object': {'number':11,'string':'abcd',}," +
                "'array': [{'number':11,'string':'abcd',}]," +
                "}");
        JSONObject json2 = JSONObject.parseObject("{" +
                "'number':1," +
                "'string':'abc'," +
                "'object': {'number':1,'string':'abc',}," +
                "'array': [{'number':1,'string':'abc',}]," +
                "}");
        System.out.println(json1);
        System.out.println(json2);
        JSONObject result = ExecuteJsonNodeUtil.compareJsonObject(json1, json2);
        System.out.println(result.toJSONString(JSONWriter.Feature.PrettyFormat));
        String expected = "{\"object.string\":{\"equal\":false,\"typeEqual\":true,\"type\":\"MODIFY\"},\"number\":{\"equal\":false,\"typeEqual\":true,\"type\":\"MODIFY\"},\"string\":{\"equal\":false,\"typeEqual\":true,\"type\":\"MODIFY\"},\"array[0].number\":{\"equal\":false,\"typeEqual\":true,\"type\":\"MODIFY\"},\"array[0].string\":{\"equal\":false,\"typeEqual\":true,\"type\":\"MODIFY\"},\"object.number\":{\"equal\":false,\"typeEqual\":true,\"type\":\"MODIFY\"}}";
        Assert.assertEquals(expected, result.toString());
    }

    public void testCompareJsonObject2() {
        JSONObject json1 = JSONObject.parseObject("{" +
                "'number':'abc'," +
                "'string':123," +
                "'object': {'number':'abc','string':123,}," +
                "'array': [{'number':'abc','string':123,}]," +
                "}");
        JSONObject json2 = JSONObject.parseObject("{" +
                "'number':1," +
                "'string':'abc'," +
                "'object': {'number':123,'string':'abc',}," +
                "'array': [{'number':123,'string':'abc',}]," +
                "}");
        System.out.println(json1);
        System.out.println(json2);
        JSONObject result = ExecuteJsonNodeUtil.compareJsonObject(json1, json2);
        System.out.println(result.toJSONString(JSONWriter.Feature.PrettyFormat));
        String expected = "{\"object.string\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"number\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"string\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"array[0].number\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"array[0].string\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"object.number\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"}}";
        Assert.assertEquals(expected, result.toString());
    }

    public void testCompareJsonObject3() {
        JSONObject json1 = JSONObject.parseObject("{" +
//                "'number':'abc'," +
                "'string':123," +
                "'object': {'number':'abc','string':123,}," +
                "'array': [{'number':'abc','string':123,}]," +
                "}");
        JSONObject json2 = JSONObject.parseObject("{" +
                "'number':1," +
//                "'string':'abc'," +
//                "'object': {'number':123,'string':'abc',}," +
                "'array': [{'number':123,'string':'abc',}]," +
                "}");
        System.out.println(json1);
        System.out.println(json2);
        JSONObject result = ExecuteJsonNodeUtil.compareJsonObject(json1, json2);
        System.out.println(result.toJSONString(JSONWriter.Feature.PrettyFormat));
        String expected = "{\"object.string\":{\"equal\":false,\"type\":\"REMOVE\"},\"number\":{\"equal\":false,\"type\":\"ADD\"},\"string\":{\"equal\":false,\"type\":\"REMOVE\"},\"array[0].number\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"array[0].string\":{\"equal\":false,\"typeEqual\":false,\"type\":\"MODIFY\"},\"object.number\":{\"equal\":false,\"type\":\"REMOVE\"}}";
        Assert.assertEquals(expected, result.toString());
    }

    public void testJsonObjectEquals() {
        JSONObject json1 = JSONObject.parseObject("{" +
                "'number':1," +
                "'string':'abc'," +
                "'object': {'number':1,'string':'abc',}," +
                "'array': [{'number':1,'string':'abc',}]," +
                "}");
        JSONObject json2 = JSONObject.parseObject("{" +
                "'number':1," +
                "'string':'abc'," +
                "'object': {'number':1,'string':'abc',}," +
                "'array': [{'number':1,'string':'abc',}]," +
                "}");
        boolean result = ExecuteJsonNodeUtil.jsonObjectEquals(json1, json2);
        System.out.println(result);
        Object expected = true;
        Assert.assertEquals(expected, result);
    }

    public void testJsonObjectEquals3() {
        {
            JSONObject json1 = JSONObject.parseObject("{" +
//                "'number':'abc'," +
                    "'string':123," +
                    "'object': {'number':'abc','string':123,}," +
                    "'array': [{'number':'abc','string':123,}]," +
                    "}");
            JSONObject json2 = JSONObject.parseObject("{" +
                    "'number':1," +
//                "'string':'abc'," +
//                "'object': {'number':123,'string':'abc',}," +
                    "'array': [{'number':123,'string':'abc',}]," +
                    "}");
            boolean result = ExecuteJsonNodeUtil.jsonObjectEquals(json1, json2);
            System.out.println(result);
            Object expected = false;
            Assert.assertEquals(expected, result);
        }
    }
}