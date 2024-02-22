package fit.lang.plugin.json;

import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import junit.framework.TestCase;
import org.junit.Assert;

import java.io.UnsupportedEncodingException;

public class FitLangExpressToolTest extends TestCase {

    public void testEncodeHex() {
        String text = "abc123上中下";
        String result = FitLangExpressTool.encodeHex(text);
        Assert.assertEquals("616263313233e4b88ae4b8ade4b88b", result);
    }

    public void testDecodeHex() {
        String text = "616263313233e4b88ae4b8ade4b88b";
        String result = FitLangExpressTool.decodeHex(text);
        Assert.assertEquals("abc123上中下", result);
    }

    public void testEncodeBase64() {
        String text = "abc123上中下";
        String result = FitLangExpressTool.encodeBase64(text);
        Assert.assertEquals("YWJjMTIz5LiK5Lit5LiL", result);
    }

    public void testDecodeBase64() {
        String text = "YWJjMTIz5LiK5Lit5LiL";
        String result = FitLangExpressTool.decodeBase64(text);
        Assert.assertEquals("abc123上中下", result);
    }

    public void testEncodeUrl() {
        String text = "abc123上中下";
        String result = FitLangExpressTool.encodeUrl(text);
        Assert.assertEquals("abc123%E4%B8%8A%E4%B8%AD%E4%B8%8B", result);
    }

    public void testDecodeUrl() {
        String text = "abc123%E4%B8%8A%E4%B8%AD%E4%B8%8B";
        String result = FitLangExpressTool.decodeUrl(text);
        Assert.assertEquals("abc123上中下", result);
    }

    public void testMd5() {
        String text = "abc123上中下";
        String result = FitLangExpressTool.md5(text);
        Assert.assertEquals("bd9f004ab3a71c521288f88cd0bcc985", result);
    }

    public void testSha1() {
        String text = "abc123上中下";
        String result = FitLangExpressTool.sha1(text);
        Assert.assertEquals("34383b719c76632dd1b4ab57851dd13a5225f359", result);
    }

    public void testUuid() {
        String result = FitLangExpressTool.uuid();
        System.out.println(result);
    }

    public void testUuidSimple() {
        String result = FitLangExpressTool.uuidSimple();
        System.out.println(result);
    }

    public void testConvertNumber() {
        String text = "123456";
        String[] expected = {
                null,
                null,
                "11110001001000000",
                "20021100110",
                "132021000",
                "12422311",
                "2351320",
                "1022634",
                "361100",
                "207313",
                "123456",
                "84833",
                "5B540",
                "44268",
                "32DC4",
                "268A6",
                "1E240",

        };
        for (int i = 2; i < 17; i++) {
            String result = FitLangExpressTool.convertNumber(text, i);
//            System.out.println("\"" + result.toUpperCase() + "\",");
            Assert.assertEquals(expected[i], result);
        }

    }

    public void testConvertCharset() throws UnsupportedEncodingException {
        String text = "中文乱码";
        String expected = "{\"GBK=>GBK\":\"中文乱码\",\"GBK=>UTF8\":\"��������\",\"GBK=>ISO8859-1\":\"ÖÐÎÄÂÒÂë\",\"UTF8=>GBK\":\"涓\uE15F枃涔辩爜\",\"UTF8=>UTF8\":\"中文乱码\",\"UTF8=>ISO8859-1\":\"ä¸\u00ADæ\u0096\u0087ä¹±ç \u0081\",\"ISO8859-1=>GBK\":\"????\",\"ISO8859-1=>UTF8\":\"????\",\"ISO8859-1=>ISO8859-1\":\"????\"}";
        JSONObject result = FitLangExpressTool.convertCharset(text);
        System.out.println(result.toString(JSONWriter.Feature.PrettyFormat));
        Assert.assertEquals(expected, result);

    }

//    public void testEncodeAesBase64() {
//        String text = "abc123上中下";
//        String key = "9MgYwmuPrjiecPMx61O6zIuy3MtIXQQ0E59T3xB6u0Gyf1gYs2i3K9Jxaa0zj4gTMazJuApwd6+jdyeI5iGHvhQyDHGVlAuYTgJrbFDrfB22Fpil2NfNnWFBTXyf7SDI";
//        String result = FitLangExpressTool.encodeAesBase64((key), text);
//        Assert.assertEquals("abc123%E4%B8%8A%E4%B8%AD%E4%B8%8B", result);
//    }
//
//    public void testDecodeAesBase64() {
//        String text = "abc123%E4%B8%8A%E4%B8%AD%E4%B8%8B";
//        String result = FitLangExpressTool.decodeUrl(text);
//        Assert.assertEquals("abc123上中下", result);
//    }

}