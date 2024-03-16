package my.lang.page.web;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.jcef.JBCefBrowser;
import com.intellij.ui.jcef.JBCefBrowserBase;
import com.intellij.ui.jcef.JBCefClient;
import com.intellij.ui.jcef.JBCefJSQuery;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class WebPageRenderPanel extends JPanel {

    VirtualFile virtualFile;

    JSONObject pageDefine;

    String url;

    JSONObject fetchData;

    JBCefBrowser browser;

    private double refreshDataInterval = 0.3;

    public WebPageRenderPanel(JSONObject pageDefine, VirtualFile virtualFile) {

        super(true);

        this.pageDefine = pageDefine;
        this.virtualFile = virtualFile;

        this.url = pageDefine.getString("url");

        this.fetchData = pageDefine.getJSONObject("fetchData");
        if (fetchData == null) {
            fetchData = new JSONObject();
        }

        browser = new JBCefBrowser();
        browser.getJBCefClient().setProperty(JBCefClient.Properties.JS_QUERY_POOL_SIZE, 1000);

        setLayout(new BorderLayout());

        addToolBar();

        add(browser.getComponent(), BorderLayout.CENTER);

        render();

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

    /**
     * 添加工具栏
     */
    void addToolBar() {
        JPanel toolBar = new JPanel();
        add(toolBar, BorderLayout.NORTH);

        JLabel fetchConfigLabel = new JLabel("fetchConfig:");
        JTextField fetchConfigText = new JTextField(20);
        fetchConfigText.setText(fetchData.toString());

        toolBar.add(fetchConfigLabel);
        toolBar.add(fetchConfigText);

        JLabel resultLabel = new JLabel("selector:");
        JTextField resultText = new JTextField(20);

        toolBar.add(resultLabel);
        toolBar.add(resultText);

        JButton fetchDataButton = new JButton("Fetch Data");
        toolBar.add(fetchDataButton);

        fetchDataButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                String jsPath = fetchConfigText.getText();
                JSONObject fetchConfig = JSONObject.parse(jsPath);
                JSONObject result = new JSONObject();

                for (String key : fetchConfig.keySet()) {

                    String selector = fetchConfig.getString(key);
                    JBCefJSQuery jsQuery = JBCefJSQuery.create((JBCefBrowserBase) browser);

                    jsQuery.addHandler((data) -> {
                        JSONObject fetchData = JSONObject.parse(data);
                        result.putAll(fetchData);

                        resultText.setText(result.toString());

                        return new JBCefJSQuery.Response(data) {
                        };
                    });
                    //window.cefQuery_2090759864_1({request: '' + 'test',onSuccess: function(response) {},onFailure: function(error_code, error_message) {}});
                    String jsInject = jsQuery.inject("fetchData");
                    String jsCode = "" +
                            "var fetchDom = document.querySelector('" + selector + "');\n" +
                            "var fetchData = JSON.stringify({'" + key + "': (fetchDom==null?'':fetchDom.textContent)});\n" +
                            "" + jsInject +
                            "";
                    browser.getCefBrowser().executeJavaScript(
                            jsCode,
                            browser.getCefBrowser().getURL(), 0
                    );
                }

            }
        });

        JButton refreshButton = new JButton("refresh");
        toolBar.add(refreshButton);
        refreshButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                browser.loadURL(url);
            }
        });

        JButton openChromeDevButton = new JButton("Open Chrome Dev");
        openChromeDevButton.addActionListener(new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                browser.openDevtools();
            }
        });
        toolBar.add(openChromeDevButton);
    }

    //http://www.hzhcontrols.com/new-1696665.html  JCEF中js与java交互、js与java相互调用
    void render() {
        browser.loadURL(url);
        browser.getComponent();
    }

    public void dispose() {
        browser.dispose();
    }
}
