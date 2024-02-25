package fit.lang.plugin.json;

import cn.hutool.core.codec.Base64;
import cn.hutool.core.lang.UUID;
import cn.hutool.core.util.HexUtil;
import cn.hutool.core.util.URLUtil;
import cn.hutool.crypto.SecureUtil;
import com.alibaba.fastjson2.JSONObject;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;

public class FitLangExpressTool {

    public static FitLangExpressTool INSTANCE = new FitLangExpressTool();

    private FitLangExpressTool() {
    }

    public static long now() {
        return System.currentTimeMillis();
    }

    public static String now(String format) {
        return new SimpleDateFormat(format).format(new Date());
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

    public static String convertNumber(Object num, Integer radix) {
        return new BigInteger(num.toString()).toString(radix).toUpperCase();
    }

    /**
     * 转换字符集
     *
     * @param text
     * @return
     * @throws UnsupportedEncodingException
     */
    public static JSONObject convertCharset(String text) throws UnsupportedEncodingException {
        if (text == null) {
            text = "";
        }
        String[] charsets = {"GBK", "UTF8", "ISO8859-1"};
        JSONObject result = new JSONObject();
        for (int i = 0; i < charsets.length; i++) {
            for (int j = 0; j < charsets.length; j++) {
                if (i == j) {
                    continue;
                }
                String fromCharset = charsets[i];
                String toCharset = charsets[j];
                String newText = new String(text.getBytes(fromCharset), toCharset);
                String key = charsets[i] + " => " + charsets[j];
                result.put(key, newText);
            }
        }
        return result;
    }

    public static JSONObject convertCharset(JSONObject jsonObject) throws UnsupportedEncodingException {
        for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
            Object value = entry.getValue();
            if (value != null) {
                value = convertCharset(value.toString());
            }
            entry.setValue(value);
        }
        return jsonObject;
    }

}
