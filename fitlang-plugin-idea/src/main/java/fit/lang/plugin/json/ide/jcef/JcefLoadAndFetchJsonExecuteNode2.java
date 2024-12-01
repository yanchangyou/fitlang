package fit.lang.plugin.json.ide.jcef;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.ui.jcef.JBCefBrowserBase;
import com.intellij.ui.jcef.JBCefJSQuery;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.define.JsonExecuteNode;
import fit.lang.plugin.json.define.JsonExecuteNodeInput;
import fit.lang.plugin.json.define.JsonExecuteNodeOutput;
import org.cef.browser.CefBrowser;
import org.cef.browser.CefFrame;
import org.cef.handler.CefLoadHandlerAdapter;

import static fit.lang.plugin.json.ide.jcef.FitJcefManager.browser;

/**
 * 执行节点
 */
public class JcefLoadAndFetchJsonExecuteNode2 extends JsonExecuteNode {

    @Override
    public void execute(JsonExecuteNodeInput input, JsonExecuteNodeOutput output) {
        JSONObject fetchSelector = nodeJsonDefine.getJSONObject("fetchSelector");
        String url = ExecuteJsonNodeUtil.parseStringField("url", input, nodeJsonDefine);
        Double refreshDataInterval = nodeJsonDefine.getDouble("refreshDataInterval");
        Integer times = nodeJsonDefine.getInteger("times");

        if (refreshDataInterval == null) {
            refreshDataInterval = 0.3;
        }
        if (times == null) {
            times = 5;
        }

        JSONObject result = fetch(input, url, fetchSelector, refreshDataInterval, times);

        output.setData(result);
    }

    private JSONObject fetch(JsonExecuteNodeInput input, String url, JSONObject fetchSelector, Double refreshDataInterval, Integer times) {
        JSONObject result = new JSONObject();
        String value = input.getString("value");

        System.out.println("open:" + url);

        browser.loadURL(url);
        browser.getJBCefClient().addLoadHandler(new CefLoadHandlerAdapter() {
            @Override
            public void onLoadEnd(CefBrowser browser, CefFrame frame, int httpStatusCode) {
                super.onLoadEnd(browser, frame, httpStatusCode);

                fetch(url, fetchSelector, refreshDataInterval, value);

            }
        }, browser.getCefBrowser());

//        try {
//            Thread.sleep(1000L);
//        } catch (InterruptedException e) {
//            throw new RuntimeException(e);
//        }


//        while (result.isEmpty()) {
//            if (times-- < 0) {
//                break;
//            }
//            try {
//                Thread.sleep(100L);
//            } catch (InterruptedException e) {
//                throw new RuntimeException(e);
//            }
//        }
        System.out.println("times:" + times);
        return result;
    }

    private static void fetch(String url, JSONObject fetchSelector, Double refreshDataInterval, String value) {
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
            String jsCode = "\n" +
                    "   var fetchDom = document.querySelector('" + selector + "');\n" +
                    "   var fetchData = JSON.stringify({\n" +
                    "       '" + key + "': (fetchDom==null?'':fetchDom.textContent),\n" +
                    "       'url': '" + url + "',\n" +
                    "       'value': '" + value + "',\n" +
                    "    });\n" +
                    "   console.info('fetchData:' + fetchData);\n" +
                    "   " + jsInject +
                    "\n";

            browser.getCefBrowser().executeJavaScript(jsCode, browser.getCefBrowser().getURL(), 0);
        }
    }
}
