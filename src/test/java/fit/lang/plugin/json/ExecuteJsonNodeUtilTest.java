package fit.lang.plugin.json;

import junit.framework.TestCase;
import org.junit.Assert;

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
}