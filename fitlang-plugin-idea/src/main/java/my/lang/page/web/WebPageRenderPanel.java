package my.lang.page.web;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.json.JsonLanguage;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.LanguageTextField;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.jcef.JBCefBrowser;
import com.intellij.ui.jcef.JBCefBrowserBase;
import com.intellij.ui.jcef.JBCefClient;
import com.intellij.ui.jcef.JBCefJSQuery;
import fit.lang.ExecuteNodeUtil;
import fit.lang.plugin.json.function.JsonPackageExecuteNode;
import fit.lang.plugin.json.ide.jcef.FitJcefManager;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;
import org.cef.browser.CefBrowser;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.*;
import static my.lang.action.RunCodeAction.implementIdeOperator;
import static my.lang.page.util.JsonPageUtil.adjustSplitPanel;

public class WebPageRenderPanel extends JPanel {

    JSONObject script;
    JSONObject uiConfig;
    Project project;

    VirtualFile virtualFile;

    JSONObject pageDefine;

    String url;

    JSONObject fetchConfig;
    JSONObject fetchResult;

    JBCefBrowser browser;

    double devAndBrowserSplitRatio = 0.35;
    double devSplitPanelRatio = 0.65;
    double configAndResultPanelRatio = 0.5;

    boolean showDev = true;

    LanguageTextField configTextEditor;
    LanguageTextField resultTextEditor;

    public WebPageRenderPanel(JSONObject webDefine, VirtualFile virtualFile, Project project) {
        super(true);
        this.project = project;

        this.pageDefine = webDefine;
        this.virtualFile = virtualFile;

        this.url = webDefine.getString("url");

        this.script = webDefine.getJSONObject("script");
        if (script == null) {
            script = JSONObject.parse("{'uni':'hello'}");
        }

        fetchConfig = webDefine.getJSONObject("fetchConfig");
        if (fetchConfig == null) {
            fetchConfig = new JSONObject();
        }
        if (!fetchConfig.containsKey("selector")) {
            fetchConfig.put("selector", new JSONObject());
        }

        this.fetchResult = webDefine.getJSONObject("fetchResult");
        if (fetchResult == null) {
            fetchResult = new JSONObject();
        }

        this.uiConfig = webDefine.getJSONObject("ui");
        if (uiConfig == null) {
            uiConfig = new JSONObject();
            webDefine.put("ui", uiConfig);
        }

        devAndBrowserSplitRatio = uiConfig.getDouble("devAndBrowserSplitRatio") != null ? uiConfig.getDouble("devAndBrowserSplitRatio") : devAndBrowserSplitRatio;
        devSplitPanelRatio = uiConfig.getDouble("devSplitPanelRatio") != null ? uiConfig.getDouble("devSplitPanelRatio") : devSplitPanelRatio;
        configAndResultPanelRatio = uiConfig.getDouble("configAndResultPanelRatio") != null ? uiConfig.getDouble("configAndResultPanelRatio") : configAndResultPanelRatio;
        showDev = uiConfig.getBoolean("showDev") != null ? uiConfig.getBoolean("showDev") : showDev;

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

        configTextEditor.setText(toJsonTextWithFormat(fetchConfig));
        resultTextEditor.setText(toJsonTextWithFormat(fetchResult));

        JSplitPane configAndResultPanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT);

        splitPane.add(devSplitPanel);
        splitPane.add(browser.getComponent());

        JPanel devPanel = new JPanel(new BorderLayout());

        if (showDev) {
            devSplitPanel.add(devPanel);

            configAndResultPanel.add(new JBScrollPane(configTextEditor));
            configAndResultPanel.add(new JBScrollPane(resultTextEditor));

            devSplitPanel.add(configAndResultPanel);

            adjustSplitPanel(devSplitPanel, devSplitPanelRatio);
            adjustSplitPanel(configAndResultPanel, configAndResultPanelRatio);

            buildDevPanel(devPanel);
        } else {
            devSplitPanel.add(new JBScrollPane(configTextEditor));
            devSplitPanel.add(new JBScrollPane(resultTextEditor));
            adjustSplitPanel(devSplitPanel, configAndResultPanelRatio);
        }

        // toolbar
        JPanel toolBar = new JPanel();
        add(toolBar, BorderLayout.NORTH);

        JButton refreshButton = new JButton("刷新");
        toolBar.add(refreshButton);
        refreshButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                browser.loadURL(url);

                if (showDev) {
                    buildDevPanel(devPanel);
                    adjustSplitPanel(devSplitPanel, devSplitPanelRatio);
                    adjustSplitPanel(configAndResultPanel, configAndResultPanelRatio);
                } else {
                    adjustSplitPanel(devSplitPanel, configAndResultPanelRatio);
                }

                adjustSplitPanel(splitPane, devAndBrowserSplitRatio);

                resultTextEditor.setText("{\n}");
            }
        });

        JLabel selectorLabel = new JLabel("selector:");
        JTextField selectorText = new JTextField(20);

        toolBar.add(selectorLabel);
        toolBar.add(selectorText);

        selectorText.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent e) {
                String selector = ExecuteNodeUtil.getClipboard();
                if (selector.contains("body")
                        || selector.contains("#")
                        || selector.contains(">")) {
                    selectorText.setText(selector);
                }
            }
        });

        JButton addSelectorButton = new JButton("添加采集");
        toolBar.add(addSelectorButton);

        addSelectorButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                String selector = selectorText.getText();
                String key = "data" + System.currentTimeMillis() % 1000;
                JSONObject config = JSONObject.parse(configTextEditor.getText());
                JSONObject fetchSelector = config.getJSONObject("selector");
                if (fetchSelector == null) {
                    fetchSelector = new JSONObject();
                }
                fetchSelector.put(key, selector);

                configTextEditor.setText(toJsonTextWithFormat(config));
            }
        });

        JButton fetchDataButton = new JButton("采集数据");
        toolBar.add(fetchDataButton);

        fetchDataButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                String configText = configTextEditor.getText();
                JSONObject fetchConfig = JSONObject.parse(configText).getJSONObject("selector");
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
                                resultTextEditor.setText(toJsonTextWithFormat(result));
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


        JButton executeButton = new JButton("执行");
        toolBar.add(executeButton);

        executeButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                JSONObject config = JSONObject.parse(configTextEditor.getText());
                JSONObject input = config.getJSONObject("input");
                JSONObject script = config.getJSONObject("script");
                if (input == null) {
                    input = new JSONObject();
                }
                if (script == null) {
                    script = new JSONObject();
                }
                execute(input, script);

            }
        });
    }

    private void execute(JSONObject input, JSONObject script) {
        try {

            FitJcefManager.setResultTextEditor(resultTextEditor);
            FitJcefManager.setBrowser(browser);

            implementIdeOperator(null, project);

            ServerJsonExecuteNode.setCurrentServerFilePath(virtualFile.getPath());
            JsonPackageExecuteNode.addImportPath(ServerJsonExecuteNode.getServerFileDir());
//
            JSONObject newContextParam = buildContextParam(project.getBasePath(), new File(virtualFile.getPath()));
//            contextParam.putAll(newContextParam);
            input.putAll(newContextParam);
//            input = parseRealFormData(input);
            //是否同步执行
//            if (isSynchronized) {
//                String result = executeCode(input, script, contextParam);
//                JSONObject output = JSONObject.parse(result);
//                output = buildOutputData(outputDefine, output);
//                setOutputJson(output);
//            } else {
            JSONObject finalInput = input;
            new Thread(() -> WriteCommandAction.runWriteCommandAction(project, () -> {
                String result = executeCode(finalInput, script, newContextParam);
                JSONObject output = JSONObject.parse(result);
                resultTextEditor.setText(toJsonTextWithFormat(output));
            })).start();
//            }
        } catch (Exception e) {
            Messages.showErrorDialog("ERROR: " + e.getLocalizedMessage(), "Error");
        }
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
