package my.lang.page.app;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;

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

        jsonNativeFormEditor = new JsonNativeFormPanel(formData);
        cardPanel.add(jsonNativeFormEditor);

//        cardLayout.next(cardPanel);

    }

    public JsonFormPanel getJsonFormEditor() {
        return jsonFormEditor;
    }

    public JSONObject getJsonObject() {
        return JSONObject.parseObject(jsonTextEditor.getText());
    }

    @Override
    protected void switchEditor() {

        if (isJsonTextEditor && showGraph) {
            JSONObject newJson = JSONObject.parse(jsonTextEditor.getText());
            jsonFormEditor.setFormDataToChrome(newJson);
        }
    }

    @Override
    public void dispose() {
        if (showGraph) {
            jsonFormEditor.close();
        }
    }
}
