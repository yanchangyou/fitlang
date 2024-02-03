package my.lang.page.app;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;

public class JsonScriptEditorPanel extends JsonBaseEditorPanel {

    JsonGraphScriptPanel jsonGraphScriptPanel;

    public JsonScriptEditorPanel(JSONObject script, String title, int horizontalAlignment, Project project) {

        super(script, title, horizontalAlignment, project);

        jsonGraphScriptPanel = new JsonGraphScriptPanel(script, jsonTextEditor);
        cardPanel.add(jsonGraphScriptPanel);

        isJsonTextEditor = true;
    }

    public JSONObject getScript() {
        return JSONObject.parseObject(jsonTextEditor.getText());
    }

    @Override
    protected void switchEditor() {

        if (isJsonTextEditor) {
            JSONObject newJson = JSONObject.parse(jsonTextEditor.getText());
            jsonGraphScriptPanel.setScriptDataToChrome(newJson);
        }
    }

    @Override
    public void dispose() {
        jsonGraphScriptPanel.close();
    }
}
