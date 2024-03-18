package my.lang.page.pick;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.json.JsonLanguage;
import com.intellij.openapi.application.ApplicationManager;
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

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;
import static my.lang.page.util.JsonPageUtil.adjustSplitPanel;

public class PickPageRenderPanel extends JPanel {

    JSONObject script;
    JSONObject uiConfig;
    Project project;

    VirtualFile virtualFile;

    JSONObject pageDefine;

    Integer pageSize;
    Integer pageNo;

    JSONArray urls;

    JSONObject fetchConfig;
    JSONObject fetchResult;

    JTextField pageNoText;
    JTextField pageSizeText;

    JBCefBrowser[] browsers;

    double devAndBrowserSplitRatio = 0.35;
    double devSplitPanelRatio = 0.65;
    double configAndResultPanelRatio = 0.5;

    LanguageTextField configTextEditor;
    LanguageTextField resultTextEditor;

    public PickPageRenderPanel(JSONObject pickDefine, VirtualFile virtualFile, Project project) {
        super(true);
        this.project = project;

        this.pageDefine = pickDefine;
        this.virtualFile = virtualFile;

        this.script = pickDefine.getJSONObject("script");
        if (script == null) {
            script = JSONObject.parse("{'uni':'hello'}");
        }

        fetchConfig = pickDefine.getJSONObject("fetchConfig");
        if (fetchConfig == null) {
            fetchConfig = new JSONObject();
        }
        if (!fetchConfig.containsKey("selector")) {
            fetchConfig.put("selector", new JSONObject());
        }

        pageNo = fetchConfig.getInteger("pageNo");
        if (pageNo == null) {
            pageNo = 1;
        }
        pageSize = fetchConfig.getInteger("pageSize");
        if (pageSize == null) {
            pageSize = 4;
        }
        urls = fetchConfig.getJSONArray("urls");
        if (urls == null) {
            urls = new JSONArray();
        }


        this.fetchResult = pickDefine.getJSONObject("fetchResult");
        if (fetchResult == null) {
            fetchResult = new JSONObject();
        }

        this.uiConfig = pickDefine.getJSONObject("ui");
        if (uiConfig == null) {
            uiConfig = new JSONObject();
            pickDefine.put("ui", uiConfig);
        }

        Integer poolSize = uiConfig.getInteger("poolSize");
        if (poolSize == null) {
            poolSize = 1000;
        }

        devAndBrowserSplitRatio = uiConfig.getDouble("devAndBrowserSplitRatio") != null ? uiConfig.getDouble("devAndBrowserSplitRatio") : devAndBrowserSplitRatio;
        devSplitPanelRatio = uiConfig.getDouble("devSplitPanelRatio") != null ? uiConfig.getDouble("devSplitPanelRatio") : devSplitPanelRatio;
        configAndResultPanelRatio = uiConfig.getDouble("configAndResultPanelRatio") != null ? uiConfig.getDouble("configAndResultPanelRatio") : configAndResultPanelRatio;

        browsers = new JBCefBrowser[pageSize];
        for (int i = 0; i < browsers.length; i++) {
            browsers[i] = new JBCefBrowser();
            browsers[i].getJBCefClient().setProperty(JBCefClient.Properties.JS_QUERY_POOL_SIZE, poolSize);
        }

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

        splitPane.add(devSplitPanel);

        JPanel browserPanel = new JPanel(new GridLayout((pageSize + 3) / 4, (pageSize + 3) / 4, 3, 3));

        splitPane.add(browserPanel);

        for (int i = 0; i < browsers.length; i++) {
            browserPanel.add(browsers[i].getComponent());
        }

        devSplitPanel.add(new JBScrollPane(configTextEditor));
        devSplitPanel.add(new JBScrollPane(resultTextEditor));
        adjustSplitPanel(devSplitPanel, configAndResultPanelRatio);


        // toolbar
        JPanel toolBar = new JPanel();
        add(toolBar, BorderLayout.NORTH);

        JLabel selectorLabel = new JLabel("selector:");
        JTextField selectorText = new JTextField(20);


        toolBar.add(selectorLabel);
        toolBar.add(selectorText);

        selectorText.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent e) {
                String selector = ExecuteNodeUtil.getClipboard();
                if ((selector.contains("body")
                        || selector.contains("#")
                        || selector.contains(">"))
                        && !selector.contains("!")
                        && !selector.contains("{")
                        && !selector.contains("}")) {
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

        JLabel pageNoLabel = new JLabel("PageNo:");
        pageNoText = new JTextField(pageNo + "", 5);

        toolBar.add(pageNoLabel);
        toolBar.add(pageNoText);

        JLabel pageSizeLabel = new JLabel("PageSize:");
        pageSizeText = new JTextField(pageSize + "", 4);

        toolBar.add(pageSizeLabel);
        toolBar.add(pageSizeText);

        JButton prePageButton = new JButton("上一页");
        toolBar.add(prePageButton);

        prePageButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                int pageNo = Integer.parseInt(pageNoText.getText());
                if (pageNo == 1) {
                    Messages.showErrorDialog("已到第1页！", "Error");
                    return;
                }

                pageNoText.setText((pageNo - 1) + "");

                render();

            }
        });

        JButton refreshButton = new JButton("刷新");
        toolBar.add(refreshButton);
        refreshButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                render();

                adjustSplitPanel(devSplitPanel, configAndResultPanelRatio);

                adjustSplitPanel(splitPane, devAndBrowserSplitRatio);
            }
        });


        JButton nextPageButton = new JButton("下一页");
        toolBar.add(nextPageButton);

        nextPageButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                int pageNo = Integer.parseInt(pageNoText.getText());
                int pageSize = Integer.parseInt(pageSizeText.getText());
                if (pageNo * pageSize + pageSize > urls.size() + pageSize - 1) {
                    Messages.showErrorDialog("超过最大页数！", "Error");
                    return;
                }

                pageNoText.setText((pageNo + 1) + "");

                render();

            }
        });

        JButton fetchDataButton = new JButton("采集数据");
        toolBar.add(fetchDataButton);

        fetchDataButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                String configText = configTextEditor.getText();
                JSONObject fetchConfig = JSONObject.parse(configText).getJSONObject("selector");

                JSONArray list = new JSONArray(browsers.length);
                int pageNo = Integer.parseInt(pageNoText.getText());
                int pageSize = Integer.parseInt(pageSizeText.getText());
                java.util.List<String> listData = new ArrayList<>();
                for (int i = pageNo * pageSize - pageSize; i < pageSize * pageNo && i < urls.size(); i++) {
                    listData.add(urls.get(i).toString());
                }

                for (int i = 0; i < listData.size(); i++) {
                    JBCefBrowser browser = browsers[i];
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
                                    JSONObject fetchData = JSONObject.parse(resultTextEditor.getText());
                                    list.add(result);
                                    fetchData.put("p" + pageNo, list);
                                    resultTextEditor.setText(toJsonTextWithFormat(fetchData));
                                }
                            });

                            return new JBCefJSQuery.Response(data) {
                            };
                        });
                        //window.cefQuery_2090759864_1({request: '' + 'test',onSuccess: function(response) {},onFailure: function(error_code, error_message) {}});
                        String jsInject = jsQuery.inject("fetchData");
                        String url = browser.getCefBrowser().getURL();
                        String jsCode = "" +
                                "var fetchDom = document.querySelector('" + selector + "');\n" +
                                "   var fetchData = JSON.stringify({\n" +
                                "       '" + key + "': (fetchDom==null?'':fetchDom.textContent),\n" +
                                "       'url': '" + url + "',\n" +
//                                "       'value': '" + value + "',\n" +
                                "    });\n" + "" + jsInject +
                                "";
                        browser.getCefBrowser().executeJavaScript(jsCode, browser.getCefBrowser().getURL(), 0);
                    }
                }
            }
        });


        adjustSplitPanel(devSplitPanel, configAndResultPanelRatio);

        adjustSplitPanel(splitPane, devAndBrowserSplitRatio);
    }

    //http://www.hzhcontrols.com/new-1696665.html  JCEF中js与java交互、js与java相互调用
    void render() {

        //获取分页数据
        int pageNo = Integer.parseInt(pageNoText.getText());
        int pageSize = Integer.parseInt(pageSizeText.getText());
        java.util.List<String> listData = new ArrayList<>();
        for (int i = pageNo * pageSize - pageSize; i < pageSize * pageNo && i < urls.size(); i++) {
            listData.add(urls.get(i).toString());
        }

        for (int i = 0; i < pageSize; i++) {
            if (i < listData.size()) {
                browsers[i].loadURL(listData.get(i));
            } else {
                browsers[i].loadHTML("<center><h2>ⓧ</h2></center>");
            }
        }
    }

    public void dispose() {
        for (JBCefBrowser browser : browsers) {
            browser.dispose();
        }
    }
}
