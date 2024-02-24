package my.lang.page.app;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.editor.ex.util.EditorUtil;
import com.intellij.openapi.project.Project;
import com.intellij.ui.LanguageTextField;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import fit.intellij.json.JsonLanguage;

import javax.swing.*;
import java.awt.*;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

/**
 * json原生form
 */
public class JsonNativeFormPanel extends JPanel {

    JSONObject formData;
    Dimension LABEL_DIMENSION = new Dimension(100, 35);

    public JsonNativeFormPanel(JSONObject formData, Project project) {

        GridLayout layout = new GridLayout(formData.keySet().size(), 1);
        setLayout(layout);
        layout.setVgap(3);
        for (String key : formData.keySet()) {
            JPanel itemPanel = new JPanel(new BorderLayout());
            JLabel label = new JLabel(key.concat("  :  "));
            label.setMinimumSize(LABEL_DIMENSION);
            label.setPreferredSize(LABEL_DIMENSION);
            label.setMaximumSize(LABEL_DIMENSION);
            label.setHorizontalAlignment(JLabel.RIGHT);
            label.setMinimumSize(new Dimension(100, 30));

            JComponent field;
            Object value = formData.get(key);
            if (value instanceof JSONObject || value instanceof JSONArray) {
                String text;
                if (value instanceof JSONObject) {
                    text = toJsonTextWithFormat((JSONObject) value);
                } else {
                    text = toJsonTextWithFormat((JSONArray) value);
                }
                LanguageTextField fieldEditor = new LanguageTextField(JsonLanguage.INSTANCE, project, text);
                fieldEditor.setFont(EditorUtil.getEditorFont());
                fieldEditor.setOneLineMode(false);
                field = fieldEditor;
            } else {
                field = new JBTextArea(formData.getString(key));
            }

            JBScrollPane jbScrollPane = new JBScrollPane(field);
            itemPanel.add(label, BorderLayout.WEST);
            itemPanel.add(jbScrollPane, BorderLayout.CENTER);
            this.add(itemPanel);
        }
    }
}
