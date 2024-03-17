package fit.lang.plugin.json.ide.jcef;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.ui.jcef.JBCefBrowser;
import com.intellij.ui.jcef.JBCefBrowserBase;
import com.intellij.ui.jcef.JBCefJSQuery;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;

import static fit.lang.plugin.json.ide.jcef.FitJcefManager.browser;

/**
 * 执行节点
 */
public class JcefFetchJsonExecuteNode extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        JSONObject fetchSelector = nodeJsonDefine.getJSONObject("fetchSelector");
        Double second = nodeJsonDefine.getDouble("second");
        if (second == null) {
            second = 1.0;
        }
        JSONArray urls = input.getJsonArray("urls");
        String url = input.getString("url");

        Double finalSecond = second;
//        try {
//            Thread.sleep((long) (finalSecond * 1000));
//        } catch (InterruptedException e) {
//            throw new RuntimeException(e);
//        }
        if (url != null) {
            JSONObject result = fetch(input, url, fetchSelector);
            output.setData(result);
        }
        if (urls != null) {
            JSONObject result = fetch(input, urls, fetchSelector);
            output.setData(result);
        }
//        try {
//            Thread.sleep((long) (finalSecond * 1000));
//        } catch (InterruptedException e) {
//            throw new RuntimeException(e);
//        }
    }

    private JSONObject fetch(JsonExecuteNodeInput input, String url, JSONObject fetchSelector) {
        JSONObject result = new JSONObject();

        for (String key : fetchSelector.keySet()) {

            String selector = fetchSelector.getString(key);
            JBCefJSQuery jsQuery = JBCefJSQuery.create((JBCefBrowserBase) browser);

            jsQuery.addHandler((data) -> {

                JSONObject fetchData = JSONObject.parse(data);

                FitJcefManager.callback(fetchData);
                System.out.println("fetch-data:" + data);

                return new JBCefJSQuery.Response(data) {
                };
            });
            //window.cefQuery_2090759864_1({request: '' + 'test',onSuccess: function(response) {},onFailure: function(error_code, error_message) {}});
            String jsInject = jsQuery.inject("fetchData");
            String jsCode = "" +
                    "var fetchDom = document.querySelector('" + selector + "');\n" +
                    "var fetchData = JSON.stringify({" +
                    "   '" + key + "': (fetchDom==null?'':fetchDom.textContent)," +
                    "   'url': '" + url + "'," +
                    "});\n" +
                    "" + jsInject +
                    "";
            browser.getCefBrowser().executeJavaScript(
                    jsCode,
                    url, 0
            );
        }
        return result;
    }

    private JSONObject fetch(JsonExecuteNodeInput input, JSONArray urls, JSONObject fetchSelector) {
        JSONObject result = new JSONObject();

        JBCefBrowser[] browsers = FitJcefManager.getBrowsers();

        for (int i = 0; i < browsers.length && i < urls.size(); i++) {

            String url = urls.getString(i);
            for (String key : fetchSelector.keySet()) {

                String selector = fetchSelector.getString(key);
                JBCefJSQuery jsQuery = JBCefJSQuery.create((JBCefBrowserBase) browser);

                jsQuery.addHandler((data) -> {

                    JSONObject fetchData = JSONObject.parse(data);

                    FitJcefManager.callback(fetchData);
                    System.out.println("fetch-data:" + data);

                    return new JBCefJSQuery.Response(data) {
                    };
                });
                //window.cefQuery_2090759864_1({request: '' + 'test',onSuccess: function(response) {},onFailure: function(error_code, error_message) {}});
                String jsInject = jsQuery.inject("fetchData");
                String jsCode = "" +
                        "var fetchDom = document.querySelector('" + selector + "');\n" +
                        "var fetchData = JSON.stringify({" +
                        "   '" + key + "': (fetchDom==null?'':fetchDom.textContent)," +
                        "   'url': '" + url + "'," +
                        "});\n" +
                        "" + jsInject +
                        "";
                browser.getCefBrowser().executeJavaScript(
                        jsCode,
                        url, 0
                );
            }
        }
        return result;
    }
}
