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

public class JsonFormPanel extends JPanel {

    JSONObject formSchema;

    JSONObject formData;

    JBCefBrowser browser;

    JBCefJSQuery jsQuery;

    private double refreshDataInterval = 0.3;

    String jsonData = "{}";

    public JsonFormPanel(JSONObject formSchema, JSONObject formData) {

        super(true);

        this.formSchema = formSchema;
        this.formData = formData;

        browser = new JBCefBrowser();
        browser.getJBCefClient().setProperty(JBCefClient.Properties.JS_QUERY_POOL_SIZE, 1000);

        setLayout(new BorderLayout());

        add(browser.getComponent(), BorderLayout.CENTER);

        render(formSchema, formData, browser);

        jsQuery = JBCefJSQuery.create((JBCefBrowserBase) browser);

        jsQuery.addHandler((data) -> {
            if (isJsonObjectText(data) && !jsonData.equals(data)) {
                jsonData = data;
                setFormData(JSONObject.parse(data));
            }
            return new JBCefJSQuery.Response(data) {
            };
        });
        new Thread() {
            @Override
            public void run() {
                super.run();
                try {
                    Thread.sleep(1000L);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
                synchronizeFormJson();
            }
        }.start();
    }

    public JSONObject getFormData() {
        return formData;
    }

    public void setFormData(JSONObject formData) {
        this.formData = formData;
    }

    public JSONObject getFormSchema() {
        return formSchema;
    }

    public void setFormSchema(JSONObject formSchema) {
        this.formSchema = formSchema;
    }

    //http://www.hzhcontrols.com/new-1696665.html  JCEF中js与java交互、js与java相互调用
    static void render(JSONObject formSchema, JSONObject formData, JBCefBrowser browser) {

        JSONObject jsonPage = new JSONObject();
        jsonPage.put("schema", formSchema);

        String path = "fit/JsonPage-jsonform.html";

        String type = "jsonform";
        String html = loadHtml(type, path);
        html = html.replace("{\"JSON_PAGE\": \"\"}", jsonPage.toJSONString());
        html = html.replace("{\"JSON_DATA\": \"\"}", formData.toJSONString());

        browser.loadHTML(html);

        browser.getComponent();
    }

    static Map<String, String> htmlMap = new HashMap<>();

    static String loadHtml(String type, String path) {
        String html = htmlMap.get(path);
        if (html == null) {
            InputStream inputStream = JsonFormPanel.class.getClassLoader().getResourceAsStream(path);
            if (inputStream != null) {
                html = IoUtil.readUtf8(inputStream);
            } else {
                html = "<h2 style='margin:50px;color: red;'>Not support page type: " + type + "</h2>";
            }
            htmlMap.put(path, html);
        }
        return html;
    }

    public void close() {
        browser.getCefBrowser().close(false);
    }


    void synchronizeFormJson() {

        //window.cefQuery_2090759864_1({request: '' + 'test',onSuccess: function(response) {},onFailure: function(error_code, error_message) {}});
        String js = jsQuery.inject("jsonData");
        browser.getCefBrowser().executeJavaScript("" +
                        " let oldJsonData = '';" +
                        " setInterval(function() {\n" +
                        "   if(getJsonData){" +
                        "       let jsonData = getJsonData();\n" +
                        "       if(oldJsonData != jsonData) {" +
                        "           oldJsonData = jsonData;" +
                        "           " + js +
                        "       }" +
                        "   }}, " + refreshDataInterval * 1000 + "); " +
                        ""
                ,
                browser.getCefBrowser().getURL(), 0
        );

    }

}
