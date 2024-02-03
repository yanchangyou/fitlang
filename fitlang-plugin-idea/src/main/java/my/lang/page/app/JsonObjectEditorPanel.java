package my.lang.page.app;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

public class JsonObjectEditorPanel extends JsonBaseEditorPanel {

    JSONObject formSchema;

    JsonFormPanel jsonFormEditor;

    public JsonObjectEditorPanel(JSONObject formSchema, JSONObject formData, String title, int horizontalAlignment, Project project) {

        super(formData, title, horizontalAlignment, project);

        this.formSchema = formSchema;

        jsonFormEditor = new JsonFormPanel(formSchema, formData);
        cardPanel.add(jsonFormEditor);

        cardLayout.next(cardPanel);

        isJsonTextEditor = false;
    }

    public JsonFormPanel getJsonFormEditor() {
        return jsonFormEditor;
    }

    public JSONObject getJsonObject() {
        if (isJsonTextEditor) {
            return JSONObject.parseObject(jsonTextEditor.getText());
        }
        return jsonFormEditor.getFormData();
    }

    @Override
    protected void switchEditor() {

        if (isJsonTextEditor) {
            JSONObject newJson = JSONObject.parse(jsonTextEditor.getText());
            jsonFormEditor.setFormDataToChrome(newJson);
        } else {
            String newJsonText = toJsonTextWithFormat(jsonFormEditor.getFormData());
            jsonTextEditor.setText(newJsonText);
        }

    }

    @Override
    public void dispose() {
        jsonFormEditor.close();
    }
}
