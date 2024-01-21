package my.lang.action.fit;

import cn.hutool.core.io.IoUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.ui.jcef.JBCefBrowser;
import com.intellij.ui.jcef.JBCefBrowserBase;
import com.intellij.ui.jcef.JBCefClient;
import com.intellij.ui.jcef.JBCefJSQuery;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.InputStream;

public class JsonPagePanel extends DialogWrapper {

    String pageType;

    /**
     * 保存数据的间隔秒数
     */
    double refreshDataInterval = 0.5;

    JSONObject jsonPage;

    JSONObject jsonData;

    JSONObject option;

    JSONObject context;

    JBCefBrowser browser;

    JBCefJSQuery jsQuery;

    /**
     * 是否模态
     */
    boolean modal;

    public JsonPagePanel(JSONObject jsonPage, JSONObject jsonData, JSONObject option, JSONObject context) {

        super(true);

        if (option == null) {
            option = new JSONObject();
        }
        if (context == null) {
            context = new JSONObject();
        }

        String title = "Web Page";
        if (option.containsKey("title")) {
            title = option.getString("title");
        }
        setTitle(title);

        this.setModal(!Boolean.FALSE.equals(option.getBoolean("modal")));

        this.jsonPage = jsonPage;
        this.jsonData = jsonData;

        this.option = option;
        this.context = context;

        pageType = option.getString("pageType");
        Double refreshDataIntervalConfig = option.getDouble("refreshDataInterval");

        if (refreshDataIntervalConfig != null) {
            refreshDataInterval = refreshDataIntervalConfig;
        }

        int width = option.getIntValue("width", 800);
        int height = option.getIntValue("height", 600);

        setSize(width, height);

        browser = new JBCefBrowser();
        browser.getJBCefClient().setProperty(JBCefClient.Properties.JS_QUERY_POOL_SIZE, 10000);

        init();

    }

    public JSONObject getJsonData() {
        return jsonData;
    }

    @Override
    public boolean isModal() {
        return modal;
    }

    //http://www.hzhcontrols.com/new-1696665.html  JCEF中js与java交互、js与java相互调用
    @Nullable
    @Override
    protected JComponent createCenterPanel() {

        String path = "fit/JsonPage.html";
        if (StrUtil.isNotBlank(pageType)) {
            path = path.replace(".html", "-" + pageType + ".html");
        }
        String html = loadHtml(path);
        html = html.replace("{\"JSON_PAGE\": \"\"}", jsonPage.toJSONString());
        html = html.replace("{\"JSON_DATA\": \"\"}", jsonData.toJSONString());
        browser.loadHTML(html);
        if (Boolean.TRUE.equals(option.getBoolean("devTools"))) {
            browser.openDevtools();
        }

        jsQuery = JBCefJSQuery.create((JBCefBrowserBase) browser);

        jsQuery.addHandler((data) -> {

            jsonData = JSONObject.parse(data);
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

        return browser.getComponent();
    }

    @Override
    public void doOKAction() {

        JsonPagePanel.super.doOKAction();

        new Thread() {
            @Override
            public void run() {
                try {
                    Thread.sleep(2 * 1000L);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
                browser.getCefBrowser().close(false);
            }
        }.start();
    }

    @Override
    public void doCancelAction() {
        super.doCancelAction();
        browser.getCefBrowser().close(true);
    }

    void synchronizeFormJson() {

        //window.cefQuery_2090759864_1({request: '' + 'test',onSuccess: function(response) {},onFailure: function(error_code, error_message) {}});
        String js = jsQuery.inject("formJson");
        browser.getCefBrowser().executeJavaScript("" +
                        " setInterval(function(){  var formJson = getFormJson();" +
                        "   " + js +
                        "   }, " + refreshDataInterval + "); " +
                        ""
                ,
                browser.getCefBrowser().getURL(), 0
        );

    }

    String loadHtml(String path) {
        InputStream inputStream = JsonPagePanel.class.getClassLoader().getResourceAsStream(path);
        return IoUtil.readUtf8(inputStream);
    }
}
