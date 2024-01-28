package fit.lang.plugin.json;

import junit.framework.TestCase;
import org.junit.Assert;

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