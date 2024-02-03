package my.lang.page.app;

import cn.hutool.core.io.IoUtil;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.ui.jcef.JBCefBrowser;
import com.intellij.ui.jcef.JBCefBrowserBase;
import com.intellij.ui.jcef.JBCefClient;
import com.intellij.ui.jcef.JBCefJSQuery;

import javax.swing.*;
import java.awt.*;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;

public class JsonGraphScriptPanel extends JPanel {

    JSONObject script;

    JBCefBrowser browser;

    JBCefJSQuery jsQuery;

    private double refreshDataInterval = 0.3;

    String jsonData = "{}";

    public JsonGraphScriptPanel(JSONObject script) {

        super(true);

        this.script = script;

        browser = new JBCefBrowser();
        browser.getJBCefClient().setProperty(JBCefClient.Properties.JS_QUERY_POOL_SIZE, 1000);

        setLayout(new BorderLayout());

        add(browser.getComponent(), BorderLayout.CENTER);

        render(script, browser);

        jsQuery = JBCefJSQuery.create((JBCefBrowserBase) browser);

        jsQuery.addHandler((data) -> {
            if (isJsonObjectText(data) && !jsonData.equals(data)) {
                jsonData = data;
            }
            return new JBCefJSQuery.Response(data) {
            };
        });


        jsQuery.addHandler((data) -> {
            if (isJsonObjectText(data) && !jsonData.equals(data)) {
                jsonData = data;
                setScript(JSONObject.parse(data));
            }
            return new JBCefJSQuery.Response(data) {
            };
        });
        new Thread() {
            @Override
            public void run() {
                super.run();
                try {
                    Thread.sleep(2000L);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
                synchronizeScriptJson();
            }
        }.start();
    }

    public JSONObject getScript() {
        return script;
    }

    public void setScript(JSONObject script) {
        this.script = script;
    }

    //http://www.hzhcontrols.com/new-1696665.html  JCEF中js与java交互、js与java相互调用
    static void render(JSONObject script, JBCefBrowser browser) {

        JSONObject jsonPage = new JSONObject();
        jsonPage.put("script", script);

        String path = "fit/JsonPage-logicflow.html";

        String type = "logicflow";
        String html = loadHtml(type, path);
        html = html.replace("{\"JSON_PAGE\": \"\"}", jsonPage.toJSONString());
//        html = html.replace("{\"JSON_DATA\": \"\"}", formData.toJSONString());

        browser.loadHTML(html);

        browser.getComponent();
    }

    static Map<String, String> htmlMap = new HashMap<>();

    static String loadHtml(String type, String path) {
        String html = htmlMap.get(path);
        if (html == null) {
            InputStream inputStream = JsonGraphScriptPanel.class.getClassLoader().getResourceAsStream(path);
            if (inputStream != null) {
                html = IoUtil.readUtf8(inputStream);
            } else {
                html = "<h2 style='margin:50px;color: red;'>Not support page type: " + type + "</h2>";
            }
            htmlMap.put(path, html);
        }
        return html;
    }


    void synchronizeScriptJson() {

        //window.cefQuery_2090759864_1({request: '' + 'test',onSuccess: function(response) {},onFailure: function(error_code, error_message) {}});
        String js = jsQuery.inject("scriptData");
        browser.getCefBrowser().executeJavaScript("" +
                        " let oldJsonData = '';" +
                        " setInterval(function() {\n" +
                        "   if(getScriptData){" +
                        "       let scriptData = getScriptData();\n" +
                        "       if(oldJsonData != scriptData) {" +
                        "           oldJsonData = scriptData;" +
                        "           " + js +
                        "       }" +
                        "   }}, " + refreshDataInterval * 1000 + "); " +
                        ""
                ,
                browser.getCefBrowser().getURL(), 0
        );

    }

    public void close() {
        jsQuery.dispose();
        browser.dispose();
    }

    public void setScriptDataToChrome(JSONObject script) {

        //window.cefQuery_2090759864_1({request: '' + 'test',onSuccess: function(response) {},onFailure: function(error_code, error_message) {}});
        browser.getCefBrowser().executeJavaScript("" +
                        " setScriptData({'script':" + script + "});" +
                        ""
                ,
                browser.getCefBrowser().getURL(), 0
        );
    }

    public JSONObject getScriptData() {
        return script;
    }
}
