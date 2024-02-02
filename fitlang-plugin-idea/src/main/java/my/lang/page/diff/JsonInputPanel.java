package my.lang.page.diff;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.json.json5.Json5Language;
import com.intellij.lang.Language;
import com.intellij.openapi.editor.ex.util.EditorUtil;
import com.intellij.openapi.fileTypes.PlainTextLanguage;
import com.intellij.openapi.project.Project;
import com.intellij.ui.LanguageTextField;
import com.intellij.ui.components.JBScrollPane;

import javax.swing.*;
import java.awt.*;

public class JsonInputPanel extends JPanel {

    Object object;

    LanguageTextField jsonTextEditor;

    Project project;

    String title;

    int horizontalAlignment;


    public JsonInputPanel(Object object, String title, int horizontalAlignment, Project project) {

        super(true);

        setLayout(new BorderLayout());

        this.object = object;
        this.project = project;
        this.title = title;
        this.horizontalAlignment = horizontalAlignment;

        Language language = PlainTextLanguage.INSTANCE;

        if (object instanceof JSONObject) {
            language = Json5Language.INSTANCE;
        }

        jsonTextEditor = new LanguageTextField(language, project, object.toString());

        jsonTextEditor.setFont(EditorUtil.getEditorFont());
        jsonTextEditor.setOneLineMode(false);

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

        add(editorPanel, BorderLayout.CENTER);
    }

}
