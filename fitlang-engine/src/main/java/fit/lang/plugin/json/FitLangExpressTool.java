package fit.lang.plugin.json;

import cn.hutool.core.codec.Base64;
import cn.hutool.core.lang.UUID;
import cn.hutool.core.util.HexUtil;
import cn.hutool.core.util.URLUtil;
import cn.hutool.crypto.SecureUtil;

public class FitLangExpressTool {

    public static FitLangExpressTool INSTANCE = new FitLangExpressTool();

    private FitLangExpressTool() {
    }

    public static String encodeHex(String text) {
        return HexUtil.encodeHexStr(text);
    }

    public static String decodeHex(String text) {
        return HexUtil.decodeHexStr(text);
    }

    public static String encodeBase64(String text) {
        return Base64.encode(text);
    }

    public static String decodeBase64(String text) {
        return Base64.decodeStr(text);
    }

    public static String encodeUrl(String text) {
        return URLUtil.encode(text);
    }

    public static String decodeUrl(String text) {
        return URLUtil.decode(text);
    }

    public static String md5(String text) {
        return SecureUtil.md5(text);
    }

    public static String sha1(String text) {
        return SecureUtil.sha1(text);
    }

    public static String uuid() {
        return UUID.randomUUID().toString();
    }

    public static String uuidSimple() {
        return UUID.randomUUID().toString(true);
    }

//
//    public static String encodeAesBase64(String text, String key) {
//        return SecureUtil.aes(Base64.decode(key)).encryptBase64(text);
//    }
//
//    public static String encodeAesHex(String text, String key) {
//        return SecureUtil.aes(key.getBytes()).encryptHex(text);
//    }
//
//    public static String decodeAesBase64(String text, String key) {
//        return SecureUtil.aes(key.getBytes()).decryptStr(text);
//    }
//
//    public static String decodeAesHex(String text, String key) {
//        return SecureUtil.aes(key.getBytes()).decryptStr(text);
//    }

}
