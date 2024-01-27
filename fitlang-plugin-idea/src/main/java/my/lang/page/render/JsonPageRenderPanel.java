package my.lang.page.render;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IoUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import com.intellij.openapi.vfs.VirtualFile;
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

public class JsonPageRenderPanel extends JPanel {

    VirtualFile virtualFile;

    JSONObject pageDefine;

    String type;

    JSONObject jsonPage;

    JBCefBrowser browser;

    JBCefJSQuery jsQuery;

    private double refreshDataInterval = 0.3;

    String jsonData = "{}";

    public JsonPageRenderPanel(JSONObject pageDefine, VirtualFile virtualFile) {

        super(true);

        this.pageDefine = pageDefine;
        this.virtualFile = virtualFile;

        this.type = pageDefine.getString("type");

        this.jsonPage = pageDefine.getJSONObject("page");

        browser = new JBCefBrowser();
        browser.getJBCefClient().setProperty(JBCefClient.Properties.JS_QUERY_POOL_SIZE, 1000);

        setLayout(new BorderLayout());

//        addToolBar();

        add(browser.getComponent(), BorderLayout.CENTER);

        jsonData = readData();

        if (StrUtil.isBlank(jsonData)) {
            jsonData = "{}";
        }

        render(type, jsonPage, JSONObject.parse(jsonData), browser);

        jsQuery = JBCefJSQuery.create((JBCefBrowserBase) browser);

        jsQuery.addHandler((data) -> {
            if (isJsonObjectText(data) && !jsonData.equals(data)) {
                jsonData = data;
                JSONObject jsonData = JSONObject.parse(data);
                writeData(jsonData);
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

    private VirtualFile getDataFileOrCreate() {
        String name = virtualFile.getName();
        String dataName = name.replace(".page.", ".");
        VirtualFile dataFile = virtualFile.getParent().findChild(dataName);
        if (dataFile == null || !dataFile.exists()) {
            FileUtil.writeUtf8String("{}", virtualFile.getPath().replace(".page.", "."));
            dataFile = virtualFile.getParent().findChild(dataName);
        }
        return dataFile;
    }

    String readData() {
        VirtualFile dataFile = getDataFileOrCreate();
        if (dataFile != null) {
            return FileUtil.readUtf8String(dataFile.getPath());
        }
        return "";
    }

    void writeData(JSONObject data) {
        writeData(data.toJSONString(JSONWriter.Feature.PrettyFormat));
    }

    void writeData(String data) {
        VirtualFile dataFile = getDataFileOrCreate();
        if (dataFile == null || !dataFile.exists()) {
            FileUtil.createTempFile();
        }
        FileUtil.writeUtf8String(data, dataFile.getPath());
        dataFile.refresh(false, false);
    }

    /**
     * 添加工具栏
     */
    void addToolBar() {
        JPanel toolBar = new JPanel();
        JButton saveDataButton = new JButton("Save Data");
        toolBar.add(saveDataButton);

        add(toolBar, BorderLayout.NORTH);
    }

    //http://www.hzhcontrols.com/new-1696665.html  JCEF中js与java交互、js与java相互调用
    static void render(String type, JSONObject jsonPage, JSONObject jsonData, JBCefBrowser browser) {

        if (jsonPage == null) {
            jsonPage = new JSONObject();
        }

        String path = "fit/JsonPage.html";

        if (StrUtil.isNotBlank(type)) {
            path = path.replace(".html", "-" + type + ".html");
        }

        String html = loadHtml(type, path);
        html = html.replace("{\"JSON_PAGE\": \"\"}", jsonPage.toJSONString());
        html = html.replace("{\"JSON_DATA\": \"\"}", jsonData.toJSONString());

        browser.loadHTML(html);

        browser.getComponent();
    }

    static Map<String, String> htmlMap = new HashMap<>();

    static String loadHtml(String type, String path) {
        String html = htmlMap.get(path);
        if (html == null) {
            InputStream inputStream = JsonPageRenderPanel.class.getClassLoader().getResourceAsStream(path);
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
