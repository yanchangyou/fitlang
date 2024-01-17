package my.lang.action;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.ui.jcef.JBCefBrowser;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class HtmlPanel extends DialogWrapper {

    String html;

    JSONObject option;

    JSONObject context;

    JBCefBrowser browser = new JBCefBrowser();

    public HtmlPanel(String html, JSONObject option, JSONObject context) {

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

        this.html = html;
        this.option = option;
        this.context = context;

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
        browser.loadHTML(html);
        return browser.getComponent();
    }

    @Override
    protected void doOKAction() {
        super.doOKAction();
        browser.getCefBrowser().close(true);
    }
}
