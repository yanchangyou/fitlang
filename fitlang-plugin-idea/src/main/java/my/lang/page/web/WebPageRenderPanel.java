package my.lang.page.web;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import com.intellij.json.JsonLanguage;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.LanguageTextField;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.jcef.JBCefBrowser;
import com.intellij.ui.jcef.JBCefBrowserBase;
import com.intellij.ui.jcef.JBCefClient;
import com.intellij.ui.jcef.JBCefJSQuery;
import org.cef.browser.CefBrowser;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import static my.lang.page.util.JsonPageUtil.adjustSplitPanel;

public class WebPageRenderPanel extends JPanel {

    JSONObject uiConfig;
    Project project;

    VirtualFile virtualFile;

    JSONObject pageDefine;

    String url;

    JSONObject fetchSelector;
    JSONObject fetchResult;

    JBCefBrowser browser;

    double devAndBrowserSplitRatio = 0.35;
    double devSplitPanelRatio = 0.6;
    double configAndResultPanelRatio = 0.5;

    LanguageTextField configTextEditor;
    LanguageTextField resultTextEditor;

    public WebPageRenderPanel(JSONObject pageDefine, VirtualFile virtualFile, Project project) {
        super(true);
        this.project = project;

        this.pageDefine = pageDefine;
        this.virtualFile = virtualFile;

        this.url = pageDefine.getString("url");

        this.fetchSelector = pageDefine.getJSONObject("fetchSelector");
        if (fetchSelector == null) {
            fetchSelector = new JSONObject();
        }

        this.fetchResult = pageDefine.getJSONObject("fetchResult");
        if (fetchResult == null) {
            fetchResult = new JSONObject();
        }

        this.uiConfig = pageDefine.getJSONObject("ui");
        if (uiConfig == null) {
            uiConfig = new JSONObject();
            devAndBrowserSplitRatio = uiConfig.getDouble("devAndBrowserSplitRatio") != null ? uiConfig.getDouble("devAndBrowserSplitRatio") : devAndBrowserSplitRatio;
            devSplitPanelRatio = uiConfig.getDouble("devSplitPanelRatio") != null ? uiConfig.getDouble("devSplitPanelRatio") : devSplitPanelRatio;
            configAndResultPanelRatio = uiConfig.getDouble("configAndResultPanelRatio") != null ? uiConfig.getDouble("configAndResultPanelRatio") : configAndResultPanelRatio;
        }

        browser = new JBCefBrowser();
        browser.getJBCefClient().setProperty(JBCefClient.Properties.JS_QUERY_POOL_SIZE, 1000);

        setLayout(new BorderLayout());

        init();

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

    void init() {
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        add(splitPane, BorderLayout.CENTER);

        JSplitPane devSplitPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);

        configTextEditor = new LanguageTextField(JsonLanguage.INSTANCE, project, "{}");
        resultTextEditor = new LanguageTextField(JsonLanguage.INSTANCE, project, "{}");
        configTextEditor.setOneLineMode(false);
        resultTextEditor.setOneLineMode(false);

        configTextEditor.setText(fetchSelector.toJSONString(JSONWriter.Feature.PrettyFormat));
        resultTextEditor.setText(fetchResult.toJSONString(JSONWriter.Feature.PrettyFormat));

        JSplitPane configAndResultPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT);

        splitPane.add(devSplitPanel);
        splitPane.add(browser.getComponent());

        adjustSplitPanel(splitPane, devAndBrowserSplitRatio);

        JPanel devPanel = new JPanel(new BorderLayout());
        devSplitPanel.add(devPanel);

        configAndResultPanel.add(new JBScrollPane(configTextEditor));
        configAndResultPanel.add(new JBScrollPane(resultTextEditor));

        devSplitPanel.add(configAndResultPanel);

        adjustSplitPanel(devSplitPanel, devSplitPanelRatio);
        adjustSplitPanel(configAndResultPanel, configAndResultPanelRatio);

        buildDevPanel(devPanel);

        // toolbar
        JPanel toolBar = new JPanel();
        add(toolBar, BorderLayout.NORTH);

        JButton refreshButton = new JButton("刷新");
        toolBar.add(refreshButton);
        refreshButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                browser.loadURL(url);

                buildDevPanel(devPanel);

                adjustSplitPanel(devSplitPanel, devSplitPanelRatio);
                adjustSplitPanel(configAndResultPanel, configAndResultPanelRatio);
                adjustSplitPanel(splitPane, devAndBrowserSplitRatio);

                resultTextEditor.setText("{\n}");
            }
        });

        JButton fetchDataButton = new JButton("采集数据");
        toolBar.add(fetchDataButton);

        fetchDataButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                String jsPath = configTextEditor.getText();
                JSONObject fetchConfig = JSONObject.parse(jsPath);
                JSONObject result = new JSONObject();

                for (String key : fetchConfig.keySet()) {

                    String selector = fetchConfig.getString(key);
                    JBCefJSQuery jsQuery = JBCefJSQuery.create((JBCefBrowserBase) browser);

                    jsQuery.addHandler((data) -> {
                        JSONObject fetchData = JSONObject.parse(data);
                        result.putAll(fetchData);

                        ApplicationManager.getApplication().invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                resultTextEditor.setText(result.toString(JSONWriter.Feature.PrettyFormat));
                            }
                        });

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
    }

    private void buildDevPanel(JPanel devPanel) {
        new Thread() {
            @Override
            public void run() {
                try {
                    Thread.sleep(1000L);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
                CefBrowser devTools = browser.getCefBrowser().getDevTools();
//                JBCefBrowser devToolsBrowser = JBCefBrowser.createBuilder()
//                        .setCefBrowser(devTools)
//                        .setClient(browser.getJBCefClient())
//                        .build();
//                devToolsBrowser.getComponent().setVisible(true);

                devPanel.removeAll();
                devPanel.add(devTools.getUIComponent());

            }
        }.start();
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
