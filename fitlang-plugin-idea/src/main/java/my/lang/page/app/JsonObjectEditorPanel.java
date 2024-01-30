package my.lang.page.app;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;

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
}
