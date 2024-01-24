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

    String type;

    JSONObject jsonPage;

    JBCefBrowser browser;

    public JsonPageRenderPanel(String type, JSONObject jsonPage) {

        super(true);

        this.type = type;

        this.jsonPage = jsonPage;

        browser = new JBCefBrowser();

        setLayout(new BorderLayout());

        JComponent component = render(type, jsonPage, browser);
        if (component != null) {
            add(component, BorderLayout.CENTER);
        }
    }

    //http://www.hzhcontrols.com/new-1696665.html  JCEF中js与java交互、js与java相互调用
    @Nullable
    protected JComponent render(String type, JSONObject jsonPage, JBCefBrowser browser) {

        String path = "fit/JsonPage.html";

        if (StrUtil.isNotBlank(type)) {
            path = path.replace(".html", "-" + type + ".html");
        }
        String html = loadHtml(path);
        html = html.replace("{\"JSON_PAGE\": \"\"}", jsonPage.toJSONString());

        browser.loadHTML(html);

        return browser.getComponent();
    }

    static Map<String, String> htmlMap = new HashMap<>();

    static String loadHtml(String path) {
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
