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
import java.util.LinkedHashMap;
import java.util.Map;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

/**
 * json原生form
 */
public class JsonNativeFormPanel extends JPanel {

    Dimension LABEL_DIMENSION = new Dimension(100, 35);

    /**
     * 需要保持顺序
     */
    Map<String, JComponent> fieldMap = new LinkedHashMap<>();

    public JsonNativeFormPanel(JSONObject formData, Project project) {
        buildForm(formData, project);
    }

    public void buildForm(JSONObject formData, Project project) {

        GridLayout layout = new GridLayout(formData.keySet().size(), 1);
        setLayout(layout);
        layout.setVgap(3);

        fieldMap.clear();
        this.removeAll();

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

            fieldMap.put(key, field);
            JBScrollPane jbScrollPane = new JBScrollPane(field);
            itemPanel.add(label, BorderLayout.WEST);
            itemPanel.add(jbScrollPane, BorderLayout.CENTER);
            this.add(itemPanel);
        }
    }

    public JSONObject getFormData() {
        JSONObject formData = new JSONObject();

        for (Map.Entry<String, JComponent> entry : fieldMap.entrySet()) {
            JComponent field = entry.getValue();
            Object value;
            if (field instanceof JBTextArea) {
                value = ((JBTextArea) field).getText();
            } else {
                String text = ((LanguageTextField) field).getText();
                if (text.startsWith("{")) {
                    value = JSONObject.parse(text);
                } else {
                    value = JSONArray.parse(text);
                }
            }
            formData.put(entry.getKey(), value);
        }
        return formData;
    }
}
