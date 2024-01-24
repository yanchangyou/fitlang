package my.lang.ide;

import cn.hutool.core.io.IoUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.ui.jcef.JBCefBrowser;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class JsonPageRenderPanel extends JPanel {

    String pageType;

    JSONObject jsonPage;

    JSONObject jsonData;

    JSONObject option;

    JSONObject context;

    JBCefBrowser browser;

    public JsonPageRenderPanel(JSONObject jsonPage, JSONObject jsonData, JSONObject option, JSONObject context) {

        super(true);

        if (option == null) {
            option = new JSONObject();
        }
        if (context == null) {
            context = new JSONObject();
        }

        this.jsonPage = jsonPage;
        this.jsonData = jsonData;

        this.option = option;
        this.context = context;

        pageType = option.getString("pageType");

        browser = new JBCefBrowser();

        JComponent component = init();

        setLayout(new BorderLayout());

        add(component, BorderLayout.CENTER);

    }

    public JSONObject getJsonData() {
        return jsonData;
    }

    //http://www.hzhcontrols.com/new-1696665.html  JCEF中js与java交互、js与java相互调用
    @Nullable
    protected JComponent init() {

        String path = "fit/JsonPage.html";

        if (StrUtil.isNotBlank(pageType)) {
            path = path.replace(".html", "-" + pageType + ".html");
        }
        String html = loadHtml(path);
        html = html.replace("{\"JSON_PAGE\": \"\"}", jsonPage.toJSONString());
        html = html.replace("{\"JSON_DATA\": \"\"}", jsonData.toJSONString());

        browser.loadHTML(html);

        return browser.getComponent();
    }

    static Map<String, String> htmlMap = new HashMap<>();

    String loadHtml(String path) {
        String html = htmlMap.get(path);
        if (html == null) {
            InputStream inputStream = JsonPageRenderPanel.class.getClassLoader().getResourceAsStream(path);
            html = IoUtil.readUtf8(inputStream);
            htmlMap.put(path, html);
        }
        return html;
    }

    public void close() {
        browser.getCefBrowser().close(false);
    }
}
