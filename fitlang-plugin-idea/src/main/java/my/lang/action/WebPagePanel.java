package my.lang.action;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.ui.jcef.JBCefBrowser;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class WebPagePanel extends DialogWrapper {

    String url;

    JSONObject option;

    JSONObject context;

    public WebPagePanel(String url, JSONObject option, JSONObject context) {

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


        this.url = StrUtil.isBlank(url) ? "http://fit.321zou.com" : url;
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
        JBCefBrowser browser = new JBCefBrowser();
        browser.loadURL(url);
        if (Boolean.TRUE.equals(option.getBoolean("devTools"))) {
            browser.openDevtools();
        }
        return browser.getComponent();
    }

    @Override
    protected void doOKAction() {
        super.doOKAction();
    }
}