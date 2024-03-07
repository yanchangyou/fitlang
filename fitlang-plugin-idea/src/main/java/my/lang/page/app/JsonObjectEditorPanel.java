package my.lang.page.app;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

public class JsonObjectEditorPanel extends JsonBaseEditorPanel {

    JSONObject formSchema;

    JsonFormPanel jsonFormEditor;

    JsonNativeFormPanel jsonNativeFormEditor;

    boolean showGraph;

    public JsonObjectEditorPanel(JSONObject formSchema, JSONObject formData, String title, int horizontalAlignment, boolean showGraph, Project project) {

        super(formData, title, horizontalAlignment, project);

        this.formSchema = formSchema;
        this.showGraph = showGraph;

        if (showGraph) {
            jsonFormEditor = new JsonFormPanel(formSchema, formData, jsonTextEditor);
            cardPanel.add(jsonFormEditor);
            isJsonTextEditor = false;
        }

        jsonNativeFormEditor = new JsonNativeFormPanel(formData, project);
        cardPanel.add(jsonNativeFormEditor);

//        cardLayout.next(cardPanel);

    }

    public void setJsonObject(JSONObject jsonObject) {
        setJsonObject(jsonObject, true);
    }

    public void setJsonObject(JSONObject jsonObject, boolean format) {
        jsonNativeFormEditor.buildForm(jsonObject, project);
        String text;
        if (format) {
            text = toJsonTextWithFormat(jsonObject);
        } else {
            text = jsonObject.toString();
        }
        jsonTextEditor.setText(text);
    }

    public JSONObject getJsonObject() {
        if (isJsonTextEditor) {
            return JSONObject.parseObject(jsonTextEditor.getText());
        }
        return jsonNativeFormEditor.getFormData();
    }

    @Override
    protected void switchEditor() {

        if (isJsonTextEditor) {
            JSONObject newJson = JSONObject.parse(jsonTextEditor.getText());
            if (showGraph) {
                jsonFormEditor.setFormDataToChrome(newJson);
            }
            jsonNativeFormEditor.buildForm(newJson, project);
        } else {
            jsonTextEditor.setText(toJsonTextWithFormat(jsonNativeFormEditor.getFormData()));
        }
    }

    @Override
    public void dispose() {
        if (showGraph) {
            jsonFormEditor.close();
        }
    }
}
