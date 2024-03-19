package my.lang.page.pick;

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

    JSONObject config;
    JSONObject fetchResult;

    JTextField pageNoText;
    JTextField pageSizeText;
    JTextField secondText;

    JPanel browserPanel;
    JBCefBrowser[] browsers;

    double devAndBrowserSplitRatio = 0.35;
    double devSplitPanelRatio = 0.65;
    double configAndResultPanelRatio = 0.5;

    LanguageTextField configTextEditor;
    LanguageTextField resultTextEditor;

    JSplitPane devSplitPanel;
    JSplitPane splitPane;

    public PickPageRenderPanel(JSONObject pickDefine, VirtualFile virtualFile, Project project) {
        super(true);
        this.project = project;

        setLayout(new BorderLayout());

        this.pageDefine = pickDefine;
        this.virtualFile = virtualFile;


        this.script = pickDefine.getJSONObject("script");
        if (script == null) {
            script = JSONObject.parse("{'uni':'hello'}");
        }

        config = pickDefine.getJSONObject("config");
        if (config == null) {
            config = new JSONObject();
        }
        if (!config.containsKey("selectorConfig")) {
            config.put("selectorConfig", new JSONObject());
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

        devAndBrowserSplitRatio = uiConfig.getDouble("devAndBrowserSplitRatio") != null ? uiConfig.getDouble("devAndBrowserSplitRatio") : devAndBrowserSplitRatio;
        devSplitPanelRatio = uiConfig.getDouble("devSplitPanelRatio") != null ? uiConfig.getDouble("devSplitPanelRatio") : devSplitPanelRatio;
        configAndResultPanelRatio = uiConfig.getDouble("configAndResultPanelRatio") != null ? uiConfig.getDouble("configAndResultPanelRatio") : configAndResultPanelRatio;

        PickConfig pickConfig = PickConfig.parse(config);

        init(pickConfig);

        render(pickConfig);
    }

    void init(PickConfig pickConfig) {

        JSONArray urls = pickConfig.getUrls();

        Integer pageSize = pickConfig.getPageSize();
        Integer pageNo = pickConfig.getPageNo();

        browsers = new JBCefBrowser[pickConfig.getGridTotal()];
        for (int i = 0; i < browsers.length; i++) {
            browsers[i] = new JBCefBrowser();
            browsers[i].getJBCefClient().setProperty(JBCefClient.Properties.JS_QUERY_POOL_SIZE, pickConfig.getPoolSize());
        }

        splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        add(splitPane, BorderLayout.CENTER);

        devSplitPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);

        configTextEditor = new LanguageTextField(JsonLanguage.INSTANCE, project, "{}");
        resultTextEditor = new LanguageTextField(JsonLanguage.INSTANCE, project, "{}");
        configTextEditor.setOneLineMode(false);
        resultTextEditor.setOneLineMode(false);

        configTextEditor.setText(toJsonTextWithFormat(config));
        resultTextEditor.setText(toJsonTextWithFormat(fetchResult));

        splitPane.add(devSplitPanel);

        browserPanel = new JPanel(new GridLayout(pickConfig.getRows(), pickConfig.getColumns(), 3, 3));

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

        JButton initButton = new JButton("初始化");
        toolBar.add(initButton);

        initButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                PickConfig pickConfig = parsePickConfig();
                reset(pickConfig);
            }
        });

        JLabel selectorLabel = new JLabel("selector:");
        JTextField selectorText = new JTextField(10);


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
                PickConfig pickConfig = parsePickConfig();

                JSONObject fetchSelector = pickConfig.getSelectorConfig();

                fetchSelector.put(key, selector);

                configTextEditor.setText(toJsonTextWithFormat(JSONObject.parseObject(JSONObject.toJSONString(pickConfig))));
            }
        });

        JLabel pageNoLabel = new JLabel("PageNo:");
        pageNoText = new JTextField(pageNo + "", 4);

        toolBar.add(pageNoLabel);
        toolBar.add(pageNoText);

        JLabel pageSizeLabel = new JLabel("PageSize:");
        pageSizeText = new JTextField(pageSize + "", 3);

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
                PickConfig pickConfig = parsePickConfig();
                render(pickConfig);

            }
        });

        JButton refreshButton = new JButton("刷新");
        toolBar.add(refreshButton);
        refreshButton.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                PickConfig pickConfig = parsePickConfig();
                render(pickConfig);

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

                PickConfig pickConfig = parsePickConfig();
                render(pickConfig);

            }
        });

        JButton fetchDataButton = new JButton("采集数据");
        toolBar.add(fetchDataButton);

        fetchDataButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                PickConfig pickConfig = parsePickConfig();
                JSONArray urls = pickConfig.getUrls();
                JSONObject selectorConfig = pickConfig.getSelectorConfig();

                JBCefBrowser[] browsers = getBrowsers();
                JSONArray list = new JSONArray(browsers.length);
                int pageNo = pickConfig.getPageNo();
                int pageSize = pickConfig.getPageSize();
                java.util.List<String> listData = new ArrayList<>();
                for (int i = pageNo * pageSize - pageSize; i < pageSize * pageNo && i < urls.size(); i++) {
                    listData.add(urls.get(i).toString());
                }

                for (int i = 0; i < listData.size(); i++) {
                    JBCefBrowser browser = browsers[i];
                    JSONObject result = new JSONObject();
                    for (String key : selectorConfig.keySet()) {

                        String selector = selectorConfig.getString(key);

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
                                "var fetchData = JSON.stringify({\n" +
                                "    '" + key + "': (fetchDom==null?'':fetchDom.textContent),\n" +
                                "    'url': '" + url + "',\n" +
                                "});\n" + "" + jsInject +
                                "";
                        browser.getCefBrowser().executeJavaScript(jsCode, browser.getCefBrowser().getURL(), 0);
                    }
                }
            }
        });

        JLabel secondLabel = new JLabel("Second:");
        secondText = new JTextField("1", 4);

        toolBar.add(secondLabel);
        toolBar.add(secondText);

        JButton continuePickButton = new JButton("连续采集");
        toolBar.add(continuePickButton);

        continuePickButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                int pageSize = Integer.parseInt(pageSizeText.getText());
                int totalPageNum = (urls.size() + pageSize - 1) / pageSize;

                double second = Double.parseDouble(secondText.getText());

                fetchDataButton.doClick();

                final int[] index = {0};
                for (int i = pageNo - 1; i < totalPageNum; i++) {
                    new Thread() {
                        @Override
                        public void run() {
                            try {
                                index[0]++;
                                Thread.sleep((long) (index[0] * second * 1000));
                            } catch (InterruptedException e) {
                                throw new RuntimeException(e);
                            }
                            fetchDataButton.doClick();
                            nextPageButton.doClick();
                        }
                    }.start();

                }
            }
        });

        JButton debugButton = new JButton("调试");
        toolBar.add(debugButton);

        debugButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                browsers[0].openDevtools();
            }
        });


        adjustSplitPanel(devSplitPanel, configAndResultPanelRatio);

        adjustSplitPanel(splitPane, devAndBrowserSplitRatio);
    }

    private PickConfig parsePickConfig() {
        String configText = configTextEditor.getText();
        PickConfig pickConfig = JSONObject.parseObject(configText, PickConfig.class);
        return pickConfig;
    }

    void reset(PickConfig pickConfig) {

        pageNoText.setText(pickConfig.getPageNo().toString());
        pageSizeText.setText(pickConfig.getPageSize().toString());
        secondText.setText(pickConfig.getSecond() + "");

        dispose();

        browsers = new JBCefBrowser[pickConfig.getGridTotal()];
        for (int i = 0; i < browsers.length; i++) {
            browsers[i] = new JBCefBrowser();
            browsers[i].getJBCefClient().setProperty(JBCefClient.Properties.JS_QUERY_POOL_SIZE, pickConfig.getPoolSize());
        }

        browserPanel.setLayout(new GridLayout(pickConfig.getRows(), pickConfig.getColumns(), 3, 3));
        browserPanel.removeAll();
        for (int i = 0; i < browsers.length; i++) {
            browserPanel.add(browsers[i].getComponent());
        }

        render(pickConfig);

        adjustSplitPanel(devSplitPanel, configAndResultPanelRatio);
        adjustSplitPanel(splitPane, devAndBrowserSplitRatio);
    }

    //http://www.hzhcontrols.com/new-1696665.html  JCEF中js与java交互、js与java相互调用
    void render(PickConfig pickConfig) {

        JSONArray urls = pickConfig.getUrls();
        //获取分页数据
        int pageNo = pickConfig.getPageNo();
        int pageSize = pickConfig.getPageSize();
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

    public JBCefBrowser[] getBrowsers() {
        return browsers;
    }
}
