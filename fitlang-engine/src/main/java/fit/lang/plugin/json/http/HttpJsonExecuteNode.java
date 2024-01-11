package fit.lang.plugin.json.http;

import cn.hutool.core.util.StrUtil;
import cn.hutool.core.util.URLUtil;
import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.HttpUtil;
import cn.hutool.http.Method;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.ExecuteNodeException;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.ExpressUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import java.net.HttpCookie;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.*;

/**
 * 执行节点
 */
public class HttpJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        String method = nodeJsonDefine.getString("method");
        if (StrUtil.isBlank(method)) {
            method = "POST";
        }
        request(input, output, nodeJsonDefine, Method.valueOf(method));
    }

    public static void request(JsonExecuteNodeInput input, JsonExecuteNodeOutput output, JSONObject nodeJsonDefine, Method method) {

        String url = ExecuteJsonNodeUtil.parseStringField("url", input, nodeJsonDefine);
        if (url == null) {
            String uni = nodeJsonDefine.getString("uni");
            if (uni.startsWith("GET http")
                    || uni.startsWith("POST http")
                    || uni.startsWith("PUT http")
                    || uni.startsWith("HEAD http")
                    || uni.startsWith("DELETE http")
            ) {
                url = uni.substring(uni.indexOf(" ") + 1);
                nodeJsonDefine.put("onlyBody", false);
            } else {
                throw new ExecuteNodeException("http node url field is required!");
            }
        }

        //add query params
        url = buildUrlByQueryParams(nodeJsonDefine, url);

        HttpRequest request = HttpUtil.createRequest(method, url);

        JSONObject header = nodeJsonDefine.getJSONObject("header");
        header = ExpressUtil.eval(header, input.getInputParamAndContextParam());
        setHttpHeader(header, request);

        JSONObject proxy = nodeJsonDefine.getJSONObject("proxy");
        proxy = ExpressUtil.eval(proxy, input.getInputParamAndContextParam());
        setProxy(proxy, request);

        JSONObject httpParam = parseParam(nodeJsonDefine);

        Boolean isPostForm = isPostForm(nodeJsonDefine);

        int retryTimes = nodeJsonDefine.getIntValue("retryTimes", 0);
        Double retrySleep = nodeJsonDefine.getDouble("retrySleep");
        String retryCondition = nodeJsonDefine.getString("retryCondition");

        if (retrySleep == null) {
            retrySleep = 0.5;
        }

        Object requestBody = null;
        if (method == Method.GET || method == Method.HEAD || (method == Method.POST && Boolean.TRUE.equals(isPostForm))) {
            requestBody = parseHttpFormParam(input, request, httpParam);
        } else if (method == Method.POST || method == Method.PUT || method == Method.DELETE) {
            String httpBody;
            if (httpParam != null) {
                Object param = ExpressUtil.eval(httpParam, input.getInputParamAndContextParam());
                if (param != null) {
                    httpBody = param.toString();
                } else {
                    httpBody = "";
                }
            } else {
                httpBody = input.getData().toJSONString();
            }
            requestBody = httpBody;
            request.body(httpBody);
        }

        long timeBegin = System.currentTimeMillis();

        JSONObject result = new JSONObject();
        HttpResponse response = null;

        int realRetryTimes = -1;

        for (int i = 0; i <= retryTimes; i++) {
            try {
                Thread.sleep((long) (i * retrySleep * 1000L));
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            response = request.execute();
            result = parseHttpResult(response);
            Object retry = ExpressUtil.eval(retryCondition, result);
            if (!Boolean.TRUE.equals(retry) && !"true".equals(retry)) {
                break;
            }
            realRetryTimes++;
        }
        long timeEnd = System.currentTimeMillis();

        JSONObject out = result;

        if (Boolean.FALSE.equals(nodeJsonDefine.get("onlyBody")) || "postman".equals(nodeJsonDefine.getString("uni"))) {
            out = new JSONObject();
            out.put("url", url);

            try {
                URL httpUrl = new URL(url);
                out.put("host", httpUrl.getHost());
                out.put("port", getUrlPort(httpUrl));
            } catch (MalformedURLException e) {
                //ignore todo
            }

            out.put("requestHeader", header);
            out.put("requestBody", requestBody);

            out.put("status", response == null ? 0 : response.getStatus());
            JSONObject headerInfo = parseHeader(response);
            out.put("header", headerInfo.getJSONObject("header"));
            out.put("cookieArray", parseCookie(response));
            out.put("cookieObject", parseCookieJson(response));
            JSONObject sizeInfo = new JSONObject();
            sizeInfo.put("header", headerInfo.getIntValue("size"));
            out.put("retryTimes", realRetryTimes);
            out.put("time", (timeEnd - timeBegin) + "ms");
            out.put("sizeInfo", sizeInfo);
            String body = response == null ? null : response.body();
            if (body != null) {
                int bodySize = response.body().length();
                sizeInfo.put("body", bodySize);
                out.put("size", bodySize + headerInfo.getIntValue("size"));
                out.put("body", result);
            }
        }
        output.setData(out);
    }


    private static Boolean isPostForm(JSONObject nodeJsonDefine) {
        Boolean isPostForm = nodeJsonDefine.getBoolean("isPostForm");
        if (Boolean.TRUE.equals(isPostForm)) {
            return true;
        }
        //再根据header的contentType判断
        JSONObject header = nodeJsonDefine.getJSONObject("header");
        if (header == null) {
            return false;
        }
        String contentType = header.getString("contentType");
        if (contentType == null) {
            return false;
        }

        return contentType.equalsIgnoreCase("application/x-www-form-urlencoded");
    }

    static JSONArray parseCookie(HttpResponse response) {
        if (response == null) {
            return new JSONArray();
        }
        return (JSONArray) JSON.toJSON(response.getCookies());
    }

    static JSONObject parseCookieJson(HttpResponse response) {
        JSONObject cookieJson = new JSONObject();
        if (response == null) {
            return new JSONObject();
        }
        List<HttpCookie> list = response.getCookies();
        for (HttpCookie cookie : list) {
            cookieJson.put(cookie.getName(), cookie.getValue());
        }
        return cookieJson;
    }

    static JSONObject parseHeader(HttpResponse response) {
        if (response == null) {
            return new JSONObject();
        }
        Map<String, List<String>> headers = response.headers();
        JSONObject headerJson = new JSONObject();
        int size = 0;
        for (Map.Entry<String, List<String>> entry : headers.entrySet()) {
            size += entry.getValue().get(0).length();
            if (entry.getKey() == null) {
                continue;
            }
            size += entry.getKey().length();
            headerJson.put(entry.getKey(), entry.getValue().get(0));
        }
        JSONObject result = new JSONObject();
        result.put("header", headerJson);
        result.put("size", size + headers.size() * 4);
        return result;
    }

    private static String buildUrlByQueryParams(JSONObject nodeJsonDefine, String url) {
        JSONObject query = nodeJsonDefine.getJSONObject("query");
        if (query != null && !query.isEmpty()) {
            String queryParams = URLUtil.buildQuery(query, Charset.defaultCharset());
            if (url.contains("?")) {
                url = url.concat("&").concat(queryParams);
            } else {
                url = url.concat("?").concat(queryParams);
            }
        }
        return url;
    }

    private static JSONObject parseParam(JSONObject nodeJsonDefine) {
        JSONObject httpParam = new JSONObject();
        String[] fields = {"param", "body"};
        for (String field : fields) {
            for (int i = 0; i < 10; i++) {
                String key = field + (i > 0 ? i + "" : "");
                Object param = nodeJsonDefine.get(key);
                if (param instanceof JSONObject) {
                    httpParam.putAll((JSONObject) param);
                }
            }
        }
        return httpParam;
    }
}
