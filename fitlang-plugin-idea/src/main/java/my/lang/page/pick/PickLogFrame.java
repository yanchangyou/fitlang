package my.lang.page.pick;

import fit.lang.ExecuteNodeUtil;

import javax.swing.*;
import java.awt.*;

public class PickLogFrame extends JFrame {

    JTextArea textArea;
    JScrollPane scrollPane;

    public PickLogFrame() {
        setSize(800, 600);
        textArea = new JTextArea("Start log at " + ExecuteNodeUtil.getNow() + "\n");
        setLayout(new BorderLayout());
        scrollPane = new JScrollPane(textArea);
        scrollPane.setSize(800, 600);
        add(scrollPane, BorderLayout.CENTER);
    }

    public synchronized void addLog(String log) {
        textArea.append(ExecuteNodeUtil.getNow() + " " + log + "\n");
    }

    public String getAllLog() {
        return textArea.getText();
    }

    public JScrollPane getScrollPane() {
        return scrollPane;
    }

    public void setScrollPane(JScrollPane scrollPane) {
        this.scrollPane = scrollPane;
    }

    public JTextArea getTextArea() {
        return textArea;
    }

    public void setTextArea(JTextArea textArea) {
        this.textArea = textArea;
    }

    public void showFrame() {
        setVisible(false);
        setVisible(true);
    }
}
