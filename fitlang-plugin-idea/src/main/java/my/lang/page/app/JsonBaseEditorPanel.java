package my.lang.page.app;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.json.json5.Json5Language;
import com.intellij.openapi.editor.ex.util.EditorUtil;
import com.intellij.openapi.project.Project;
import com.intellij.ui.LanguageTextField;
import com.intellij.ui.components.JBScrollPane;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

public abstract class JsonBaseEditorPanel extends JPanel {

    JSONObject jsonObject;

    LanguageTextField jsonTextEditor;

    Project project;

    String title;

    int horizontalAlignment;

    JPanel cardPanel;

    CardLayout cardLayout = new CardLayout();

    boolean isJsonTextEditor;

    public JsonBaseEditorPanel(JSONObject jsonObject, String title, int horizontalAlignment, Project project) {

        super(true);

        setLayout(new BorderLayout());

        this.jsonObject = jsonObject;
        this.project = project;
        this.title = title;
        this.horizontalAlignment = horizontalAlignment;

        jsonTextEditor = new LanguageTextField(Json5Language.INSTANCE, project, toJsonTextWithFormat(jsonObject));
        jsonTextEditor.setFont(EditorUtil.getEditorFont());
        jsonTextEditor.setOneLineMode(false);

        cardPanel = new JPanel(cardLayout);

        addJsonEditorPanel();


    }

    public LanguageTextField getJsonTextEditor() {
        return jsonTextEditor;
    }

    public Project getProject() {
        return project;
    }

    public void setProject(Project project) {
        this.project = project;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    protected void addJsonEditorPanel() {

        JPanel editorPanel = new JPanel(new BorderLayout());

        JBScrollPane jbScrollPane = new JBScrollPane(jsonTextEditor);

        // 第一个加入，方便获取
        editorPanel.add(jbScrollPane, BorderLayout.CENTER);

        JLabel label = new JLabel("  " + title + "  ", horizontalAlignment);
        label.setFont(EditorUtil.getEditorFont());
        editorPanel.add(label, BorderLayout.NORTH);

        cardPanel.add(jbScrollPane);

        editorPanel.add(cardPanel, BorderLayout.CENTER);

        add(editorPanel, BorderLayout.CENTER);

        label.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                cardLayout.next(cardPanel);
                switchEditor();
                isJsonTextEditor = !isJsonTextEditor;
            }
        });

    }

    protected abstract void switchEditor();

    public abstract void dispose();
}