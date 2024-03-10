package my.lang.page.app;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.editor.ex.util.EditorUtil;
import com.intellij.openapi.project.Project;
import com.intellij.ui.LanguageTextField;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;
import com.intellij.ui.components.JBTextField;
import com.intellij.util.ui.JBEmptyBorder;
import com.intellij.util.ui.JBUI;
import fit.intellij.json.JsonLanguage;
import my.lang.page.field.FileFieldComponent;

import javax.swing.*;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.LinkedHashMap;
import java.util.Map;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

/**
 * json原生form
 */
public class JsonNativeFormPanel extends JPanel {

    Dimension LABEL_DIMENSION = new Dimension(110, 35);
    Dimension LABEL_DIMENSION_MAX = new Dimension(120, 35);

    /**
     * 需要保持顺序
     */
    Map<String, JComponent> fieldMap = new LinkedHashMap<>();
    Map<String, Class<?>> fieldClass = new LinkedHashMap<>();

    public JsonNativeFormPanel(JSONObject formData, Project project) {
        buildForm(formData, project);
    }

    public void buildForm(JSONObject formData, Project project) {

        GridBagLayout layout = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        setLayout(layout);

        fieldMap.clear();
        this.removeAll();

        int index = 0;
        for (String key : formData.keySet()) {
            String labelTitle = key.split("\\$")[0];
            if (key.contains("$3")) {
                labelTitle = key.split("\\$3")[1].split("\\$")[0];
            }
            String editType = "default";
            if (key.contains("$2")) {
                editType = key.split("\\$2")[1].split("\\$")[0];
            }
            String dataType = "default";
            if (key.contains("$1")) {
                dataType = key.split("\\$1")[1].split("\\$")[0];
            }
            JPanel itemPanel = new JPanel(new BorderLayout());
            JLabel label = new JBLabel(labelTitle.concat(": "));
            label.setMinimumSize(LABEL_DIMENSION);
            label.setPreferredSize(LABEL_DIMENSION);
            label.setMaximumSize(LABEL_DIMENSION_MAX);

            label.setHorizontalAlignment(JLabel.RIGHT);
            label.setFont(EditorUtil.getEditorFont());

            JComponent field;
            Object value = formData.get(key);
            //默认空字符串
            if (value == null) {
                if ("boolean".equals(dataType)) {
                    value = false;
                } else if ("number".equals(dataType)) {
                    value = 0.0;
                } else if ("int".equals(dataType)) {
                    value = 0;
                } else {
                    value = "";
                }
            }
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
                String text = value.toString();

                if (value instanceof Number || "number".equals(dataType)) {
                    field = buildNumberInput(text);
                } else if (value instanceof Boolean || "boolean".equals(dataType)) {
                    field = new JCheckBox("", (boolean) value);
                } else if ("password".equals(editType)) {
                    field = new JPasswordField(text);
                } else if ("file".equals(editType)) {
                    field = new FileFieldComponent(text, project);
                } else if ("text".equals(editType)) {
                    field = new JBTextArea(text);
                } else {
                    field = new JBTextField(text);
                }
            }
            label.setLabelFor(field);

            field.setBorder(new JBEmptyBorder(0));

            fieldMap.put(key, field);
            fieldClass.put(key, value.getClass());

            gbc.anchor = GridBagConstraints.EAST;
            gbc.fill = GridBagConstraints.BOTH;

            gbc.weightx = 10;

            if (field instanceof LanguageTextField) {
                gbc.weighty = 10;
            } else if (field instanceof JBTextField) {
                gbc.ipady = 1;
            } else {
                gbc.ipady = 1;
            }

            gbc.gridx = 0;
            gbc.gridy = index++;

            gbc.insets = JBUI.insets(5);

            JBScrollPane jbScrollPane = new JBScrollPane(field);
            itemPanel.add(label, BorderLayout.WEST);
            itemPanel.add(jbScrollPane, BorderLayout.CENTER);
            this.add(itemPanel, gbc);
        }
    }

    private static JBTextField buildNumberInput(String text) {
        JBTextField numberField = new JBTextField(text);
        InputVerifier inputVerifier = new InputVerifier() {
            @Override
            public boolean verify(JComponent jComponent) {
                String text = numberField.getText();
                return text.matches("^[+-]?\\d*\\d(\\.\\d*)?$");
            }
        };
        numberField.setInputVerifier(inputVerifier);
        numberField.setVerifyInputWhenFocusTarget(true);
        return numberField;
    }

    public JSONObject getFormData() {
        JSONObject formData = new JSONObject();

        for (Map.Entry<String, JComponent> entry : fieldMap.entrySet()) {
            JComponent field = entry.getValue();
            String key = entry.getKey();
            Object value;
            if (field instanceof JCheckBox) {
                value = ((JCheckBox) field).isSelected();
            } else if (field instanceof FileFieldComponent) {
                value = ((FileFieldComponent) field).getText();
            } else if (field instanceof JTextComponent) {
                String text = ((JTextComponent) field).getText();
                Class<?> clazz = fieldClass.get(key);
                if (String.class.equals(clazz)) {
                    value = text;
                } else if (Boolean.class.equals(clazz)) {
                    value = Boolean.valueOf(text);
                } else if (Integer.class.equals(clazz) || BigInteger.class.equals(clazz)) {
                    value = new BigInteger(text);
                } else {
                    value = new BigDecimal(text);
                }
            } else {
                String text = ((LanguageTextField) field).getText();
                if (text.startsWith("{")) {
                    value = JSONObject.parse(text);
                } else {
                    value = JSONArray.parse(text);
                }
            }
            formData.put(key, value);
        }
        return formData;
    }
}
