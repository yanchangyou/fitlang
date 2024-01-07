package my.lang.action;

import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.ui.jcef.JBCefBrowser;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class NodePageDialogWrapper extends DialogWrapper {

    public NodePageDialogWrapper() {
        super(true);
        setTitle("Node Page");
        this.setModal(false);
        init();

//        setOKActionEnabled(false);
    }

    @Nullable
    @Override
    protected JComponent createCenterPanel() {
        JBCefBrowser browser = new JBCefBrowser();
        browser.loadURL("https://www.baidu.com");
//        browser.openDevtools();
        return browser.getComponent();
    }

    @Override
    protected void doOKAction() {
        super.doOKAction();
    }
}
