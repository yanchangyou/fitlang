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
import my.lang.page.web.DealFetchResult;
import my.lang.page.web.EmptyDealFetchResult;
import org.apache.commons.collections.set.SynchronizedSet;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

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
    boolean isStop;

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

        JSONArray checkFields = config.getJSONArray("checkFields");
        if (checkFields == null) {
            checkFields = new JSONArray();
            config.put("checkFields", checkFields);
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
        JTextField selectorText = new JTextField(6);


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

        JButton fetchDataButton = new JButton("采集数据");
        toolBar.add(fetchDataButton);

        fetchDataButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                fetchData(new EmptyDealFetchResult());
            }
        });

        JLabel secondLabel = new JLabel("Second:");
        secondText = new JTextField(pickConfig.getSecond().toString(), 4);

        toolBar.add(secondLabel);
        toolBar.add(secondText);

        JButton continuePickButton = new JButton("连续采集");
        toolBar.add(continuePickButton);

        continuePickButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                PickConfig pickConfig = parsePickConfig();
                double second = Double.parseDouble(secondText.getText());
                isStop = false;
                new Thread() {
                    @Override
                    public void run() {
                        final int[] index = {pickConfig.getGridTotal() - 1};

                        Map<String, Integer> urlRetryTimesMap = new HashMap<>();
                        Set<String> fetchOkSet = SynchronizedSet.decorate(new HashSet<>());

                        //初始化第1页
                        refreshButton.doClick();
                        try {
                            Thread.sleep((long) (second * 1000 * 4));
                        } catch (InterruptedException e) {
                            throw new RuntimeException(e);
                        }

                        //循环：采集、加载
                        while (!isStop && index[0] - pickConfig.getGridTotal() + 1 < pickConfig.getUrls().size()) {
                            fetchData(new DealFetchResult() {
                                @Override
                                public boolean isSuccess(JBCefBrowser browser) {
                                    return fetchOkSet.contains(browser.getCefBrowser().getURL());
                                }

                                @Override
                                public boolean checkData(JSONObject data, JBCefBrowser browser) {
                                    //重试次数限制，超过5次放弃
                                    String url = browser.getCefBrowser().getURL();

                                    Integer fetchTimes = urlRetryTimesMap.get(url);
                                    if (fetchTimes == null) {
                                        fetchTimes = 0;
                                    }
                                    System.out.println("1:检查页面数据：" + url + " " + fetchTimes + "次");

                                    urlRetryTimesMap.put(url, ++fetchTimes);
                                    if (fetchTimes > 5) {
                                        return true;
                                    }

                                    JSONObject fetchData = data.getJSONObject(url);
                                    if (fetchData == null) {
                                        return false;
                                    }
                                    for (Object key : pickConfig.getCheckFields()) {
                                        String value = fetchData.getString(key.toString());
                                        if ("".equals(value)) {
                                            return false;
                                        }
                                    }
                                    System.out.println("2:开始抓取数据：" + url);
                                    return true;
                                }

                                @Override
                                public void doNext(JSONObject data, JBCefBrowser browser) {
                                    String url = browser.getCefBrowser().getURL();
                                    System.out.println("3:成功抓取数据：" + url);
                                    if (!fetchOkSet.contains(url)) {
                                        fetchOkSet.add(url);
                                        index[0]++;
                                        if (index[0] < pickConfig.urls.size()) {
                                            url = pickConfig.getUrls().get(index[0]).toString();
                                            browser.loadURL(url);
                                            System.out.println("4:加载页面：" + url);
                                        }
                                    }
                                }
                            });
                            try {
                                Thread.sleep((long) (second * 1000));
                            } catch (InterruptedException e) {
                                throw new RuntimeException(e);
                            }
                        }
                        ApplicationManager.getApplication().invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                Messages.showInfoMessage("fetch OK!", "Info");
                            }
                        });
                    }
                }.start();
            }
        });

        JButton stopButton = new JButton("停止");
        toolBar.add(stopButton);

        stopButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                isStop = true;
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

    synchronized void dataToResult(JSONObject thisFetchData, PickConfig pickConfig) {

        JSONObject result = JSONObject.parse(resultTextEditor.getText());
        JSONArray fetchArray = result.getJSONArray("list");
        if (fetchArray == null) {
            fetchArray = new JSONArray();
            result.put("list", fetchArray);
        }
        JSONObject fetchData = arrayToObject(fetchArray, "url");
        fetchData.putAll(thisFetchData);
        fetchArray = objectToArray(fetchData, pickConfig.getUrls());
        result.put("list", fetchArray);
        resultTextEditor.setText(toJsonTextWithFormat(result));
    }

    private void fetchData(DealFetchResult callback) {
        PickConfig pickConfig = parsePickConfig();
        JSONObject selectorConfig = pickConfig.getSelectorConfig();

        JBCefBrowser[] browsers = getBrowsers();

        for (int i = 0; i < browsers.length; i++) {
            JBCefBrowser browser = browsers[i];

            if (browser.getCefBrowser().getURL().contains("jbcefbrowser")) {
                continue;
            }

            if (callback.isSuccess(browser)) {
                continue;
            }

            JBCefJSQuery jsQuery = JBCefJSQuery.create((JBCefBrowserBase) browser);

            jsQuery.addHandler((data) -> {
                JSONObject thisFetchData = JSONObject.parse(data);
                for (String key : thisFetchData.keySet()) {
                    JSONObject jsonObject = thisFetchData.getJSONObject(key);
                    for (String key2 : jsonObject.keySet()) {
                        jsonObject.put(key2, jsonObject.get(key2).toString()
                                .replaceAll(" +", " ")
                                .replaceAll("\n+", "\n")
                                .replaceAll("(\n )+", "\n"));
                    }
                }
                if (callback.checkData(thisFetchData, browser)) {
                    ApplicationManager.getApplication().invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            dataToResult(thisFetchData, pickConfig);
                            callback.doNext(thisFetchData, browser);
                        }
                    });
                }

                return new JBCefJSQuery.Response(data) {
                };
            });
            //window.cefQuery_2090759864_1({request: '' + 'test',onSuccess: function(response) {},onFailure: function(error_code, error_message) {}});
            String jsInject = jsQuery.inject("thisFetchData");
            String url = browser.getCefBrowser().getURL();
            String jsCode = "\n" +
                    "var selectorConfig = " + selectorConfig + ";\n" +
                    "var fetchData = {'url':'" + url + "'};\n" +
                    "for(var key in selectorConfig) {" +
                    "   var fetchDom = document.querySelector(selectorConfig[key]);\n" +
                    "   fetchData[key] = (fetchDom==null?'':fetchDom.textContent.trim());\n" +
                    "}\n" +
                    "var thisFetchData= JSON.stringify({'" + url + "':fetchData});\n" +
                    "console.info(thisFetchData);\n" +
                    "\n" + jsInject +
                    "\n";
            browser.getCefBrowser().executeJavaScript(jsCode, browser.getCefBrowser().getURL(), 0);
        }
    }

    JSONObject arrayToObject(JSONArray array, String keyField) {
        JSONObject jsonObject = new JSONObject();
        for (Object item : array) {
            JSONObject itemObject = (JSONObject) item;
            String key = itemObject.getString(keyField);
            jsonObject.put(key, itemObject);
        }
        return jsonObject;
    }

    JSONArray objectToArray(JSONObject jsonObject, JSONArray keyOrders) {
        JSONArray array = new JSONArray(jsonObject.size());
        for (Object key : keyOrders) {
            if (jsonObject.containsKey(key)) {
                array.add(jsonObject.get(key));
            }
        }
        return array;
    }

    private PickConfig parsePickConfig() {
        String configText = configTextEditor.getText();
        PickConfig pickConfig = JSONObject.parseObject(configText, PickConfig.class);
        double second = Double.parseDouble(secondText.getText());
        pickConfig.setSecond(second);

        return pickConfig;
    }

    void reset(PickConfig pickConfig) {

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

        for (int i = 0; i < pickConfig.getGridTotal(); i++) {
            if (i < urls.size()) {
                browsers[i].loadURL(urls.getString(i));
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
