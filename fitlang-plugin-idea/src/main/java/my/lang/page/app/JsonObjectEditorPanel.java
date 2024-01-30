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

public class JsonObjectEditorPanel extends JPanel {

    JSONObject formSchema;

    JSONObject formData;

    JsonFormPanel jsonFormEditor;

    LanguageTextField jsonTextEditor;

    Project project;

    String title;

    int horizontalAlignment;

    CardLayout cardLayout = new CardLayout();

    public JsonObjectEditorPanel(JSONObject formSchema, JSONObject formData, String title, int horizontalAlignment, Project project) {

        super(true);

        setLayout(new BorderLayout());

        this.formSchema = formSchema;
        this.formData = formData;
        this.project = project;
        this.title = title;
        this.horizontalAlignment = horizontalAlignment;

        addJsonEditorPanel();

    }

    public JSONObject getFormSchema() {
        return formSchema;
    }

    public void setFormSchema(JSONObject formSchema) {
        this.formSchema = formSchema;
    }

    public JSONObject getFormData() {
        return formData;
    }

    public void setFormData(JSONObject formData) {
        this.formData = formData;
    }

    public JsonFormPanel getJsonFormEditor() {
        return jsonFormEditor;
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

    private void addJsonEditorPanel() {

        JPanel editorPanel = new JPanel(new BorderLayout());

        jsonTextEditor = new LanguageTextField(Json5Language.INSTANCE, project, toJsonTextWithFormat(formData));
        jsonTextEditor.setFont(EditorUtil.getEditorFont());
        jsonTextEditor.setOneLineMode(false);

        JBScrollPane jbScrollPane = new JBScrollPane(jsonTextEditor);

        // 第一个加入，方便获取
        editorPanel.add(jbScrollPane, BorderLayout.CENTER);

        JLabel label = new JLabel("  " + title + "  ", horizontalAlignment);
        label.setFont(EditorUtil.getEditorFont());
        editorPanel.add(label, BorderLayout.NORTH);

        JPanel panel = new JPanel(cardLayout);

        jsonFormEditor = new JsonFormPanel(formSchema, formData);

        panel.add(jsonFormEditor);
        panel.add(jbScrollPane);

        editorPanel.add(panel, BorderLayout.CENTER);

        add(editorPanel, BorderLayout.CENTER);

        label.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                cardLayout.next(panel);
            }
        });

    }
}
