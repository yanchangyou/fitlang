//package my.lang.action.fit;
//
//import cn.hutool.core.io.IoUtil;
//import cn.hutool.core.util.StrUtil;
//import com.alibaba.fastjson2.JSONObject;
//import com.intellij.ui.jcef.*;
//import org.jetbrains.annotations.Nullable;
//
//import javax.swing.*;
//import java.io.InputStream;
//import java.util.HashMap;
//import java.util.Map;
//import java.util.Stack;
//
//public class JsonPageAmisHtml extends JCEFHtmlPanel {
//
//    String pageType;
//
//    /**
//     * 保存数据的间隔秒数
//     */
//    double refreshDataInterval = 0.5;
//
//    JSONObject jsonPage;
//
//    JSONObject jsonData;
//
//    JSONObject option;
//
//    JSONObject context;
//
//    JBCefBrowser browser;
//
//    JBCefJSQuery jsQuery;
//
//    static Stack<JBCefBrowser> jbCefBrowserPools = new Stack<>();
//
//    /**
//     * 是否模态
//     */
//    boolean modal;
//
//    static {
//        initBrowserPools(5);
//    }
//
//    public JsonPageAmisHtml(JSONObject jsonPage, JSONObject jsonData, JSONObject option, JSONObject context) {
//        super("");
//
//        if (option == null) {
//            option = new JSONObject();
//        }
//        if (context == null) {
//            context = new JSONObject();
//        }
//
//        String title = "Web Page";
//        if (option.containsKey("title")) {
//            title = option.getString("title");
//        }
////        setTitle(title);
//
////        this.setModal(!Boolean.FALSE.equals(option.getBoolean("modal")));
//
//        this.jsonPage = jsonPage;
//        this.jsonData = jsonData;
//
//        this.option = option;
//        this.context = context;
//
//        pageType = option.getString("pageType");
//        Double refreshDataIntervalConfig = option.getDouble("refreshDataInterval");
//
//        if (refreshDataIntervalConfig != null) {
//            refreshDataInterval = refreshDataIntervalConfig;
//        }
//
////        int width = option.getIntValue("width", 800);
////        int height = option.getIntValue("height", 600);
//
////        setSize(width, height);
//
//        if (jbCefBrowserPools.isEmpty()) {
//            initBrowserPools(3);
//        }
//        browser = jbCefBrowserPools.pop();
//
//        JComponent component = init();
//
//        component.setSize(getSize());
//
//        add(component);
//
//    }
//
//    private static void initBrowserPools(int size) {
//        for (int i = 0; i < size; i++) {
//            JBCefBrowser browser = new JBCefBrowser();
//            browser.getJBCefClient().setProperty(JBCefClient.Properties.JS_QUERY_POOL_SIZE, 1000);
//            jbCefBrowserPools.push(browser);
//        }
//    }
//
//    public JSONObject getJsonData() {
//        return jsonData;
//    }
//
//    //http://www.hzhcontrols.com/new-1696665.html  JCEF中js与java交互、js与java相互调用
//    @Nullable
//    protected JComponent init() {
//
//        String path = "fit/JsonPage.html";
//        if (StrUtil.isNotBlank(pageType)) {
//            path = path.replace(".html", "-" + pageType + ".html");
//        }
//        String html = loadHtml(path);
//        html = html.replace("{\"JSON_PAGE\": \"\"}", jsonPage.toJSONString());
//        html = html.replace("{\"JSON_DATA\": \"\"}", jsonData.toJSONString());
//        browser.loadHTML(html);
//        if (Boolean.TRUE.equals(option.getBoolean("devTools"))) {
//            browser.openDevtools();
//        }
//
//        jsQuery = JBCefJSQuery.create((JBCefBrowserBase) browser);
//
//        jsQuery.addHandler((data) -> {
//
//            jsonData = JSONObject.parse(data);
//            return new JBCefJSQuery.Response(data) {
//            };
//        });
//        new Thread() {
//            @Override
//            public void run() {
//                super.run();
//                try {
//                    Thread.sleep(1000L);
//                } catch (InterruptedException e) {
//                    throw new RuntimeException(e);
//                }
//                synchronizeFormJson();
//            }
//        }.start();
//
//        return browser.getComponent();
//    }
//
//    void synchronizeFormJson() {
//
//        //window.cefQuery_2090759864_1({request: '' + 'test',onSuccess: function(response) {},onFailure: function(error_code, error_message) {}});
//        String js = jsQuery.inject("formJson");
//        browser.getCefBrowser().executeJavaScript("" +
//                        " setInterval(function(){  var formJson = getFormJson();" +
//                        "   " + js +
//                        "   }, " + refreshDataInterval * 1000 + "); " +
//                        ""
//                ,
//                browser.getCefBrowser().getURL(), 0
//        );
//
//    }
//
//    static Map<String, String> htmlMap = new HashMap<>();
//
//    String loadHtml(String path) {
//        String html = htmlMap.get(path);
//        if (html == null) {
//            InputStream inputStream = JsonPageAmisHtml.class.getClassLoader().getResourceAsStream(path);
//            html = IoUtil.readUtf8(inputStream);
//            htmlMap.put(path, html);
//        }
//        return html;
//    }
//
//
//    public void close() {
//
//        jsQuery.clearHandlers();
//
//        //只保留5个缓存
//        if (jbCefBrowserPools.size() <= 5) {
//            browser.loadHTML("<div></div>");
//            jbCefBrowserPools.push(browser);
//        } else {
//            new Thread() {
//                @Override
//                public void run() {
//                    try {
//                        Thread.sleep(2 * 1000L);
//                    } catch (InterruptedException e) {
//                        throw new RuntimeException(e);
//                    }
//                    browser.getCefBrowser().close(false);
//                }
//            }.start();
//        }
//    }
//}