package my.lang.page.app;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;
import my.lang.page.diff.JsonDiffResultPanel;

public class JsonScriptEditorPanel extends JsonBaseEditorPanel {

    JsonGraphScriptPanel jsonGraphScriptPanel;

    JsonDiffResultPanel jsonDiffResultPanel;

    boolean enableGraph;

    public JsonScriptEditorPanel(JSONObject script, String title, int horizontalAlignment, boolean enableGraph, Project project) {

        super(script, title, horizontalAlignment, project);
        this.enableGraph = enableGraph;
        if (enableGraph) {
            jsonGraphScriptPanel = new JsonGraphScriptPanel(script, jsonTextEditor);
            cardPanel.add(jsonGraphScriptPanel);
            isJsonTextEditor = true;
        }

        jsonDiffResultPanel = new JsonDiffResultPanel(project);
        cardPanel.add(jsonDiffResultPanel);

    }

    public JSONObject getScript() {
        return JSONObject.parseObject(jsonTextEditor.getText());
    }

    @Override
    protected void switchEditor() {

        if (isJsonTextEditor && enableGraph) {
            JSONObject newJson = JSONObject.parse(jsonTextEditor.getText());
            jsonGraphScriptPanel.setScriptDataToChrome(newJson);
        }
    }

    @Override
    public void dispose() {
        if (enableGraph) {
            jsonGraphScriptPanel.dispose();
        }
    }
}
