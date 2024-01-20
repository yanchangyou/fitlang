package my.lang.action.fit;

import cn.hutool.core.io.IoUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.ui.jcef.JBCefBrowser;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.InputStream;

public class JsonPagePanel extends DialogWrapper {

    String pageType;

    JSONObject jsonPage;

    JSONObject jsonData;

    JSONObject option;

    JSONObject context;

    JBCefBrowser browser = new JBCefBrowser();

    public JsonPagePanel(JSONObject jsonPage, JSONObject jsonData, JSONObject option, JSONObject context) {

        super(true);

        if (option == null) {
            option = new JSONObject();
        }
        if (context == null) {
            context = new JSONObject();
        }

        String title = "Web Page";
        if (option.containsKey("title")) {
            title = option.getString("title");
        }
        setTitle(title);

        this.setModal(!Boolean.FALSE.equals(option.getBoolean("modal")));

        this.jsonPage = jsonPage;
        this.jsonData = jsonData;

        this.option = option;
        this.context = context;

        pageType = option.getString("pageType");

        int width = option.getIntValue("width", 800);
        int height = option.getIntValue("height", 600);

        setSize(width, height);

        init();

    }

    @Nullable
    @Override
    protected JComponent createCenterPanel() {
        if (Boolean.TRUE.equals(option.getBoolean("devTools"))) {
            browser.openDevtools();
        }
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

    @Override
    protected void doOKAction() {
        super.doOKAction();
        browser.getCefBrowser().close(true);
    }

    String loadHtml(String path) {
        InputStream inputStream = JsonPagePanel.class.getClassLoader().getResourceAsStream(path);
        return IoUtil.readUtf8(inputStream);
    }
}
