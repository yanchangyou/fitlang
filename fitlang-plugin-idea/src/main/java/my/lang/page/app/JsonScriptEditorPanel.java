package my.lang.page.app;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;
import my.lang.page.diff.JsonDiffResultPanel;

public class JsonScriptEditorPanel extends JsonBaseEditorPanel {

    JsonGraphScriptPanel jsonGraphScriptPanel;

    JsonDiffResultPanel jsonDiffResultPanel;

    boolean showGraph;

    public JsonScriptEditorPanel(JSONObject script, String title, int horizontalAlignment, boolean showGraph, Project project) {

        super(script, title, horizontalAlignment, project);
        this.showGraph = showGraph;
        if (showGraph) {
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

        if (isJsonTextEditor && showGraph) {
            JSONObject newJson = JSONObject.parse(jsonTextEditor.getText());
            jsonGraphScriptPanel.setScriptDataToChrome(newJson);
        }
    }

    @Override
    public void dispose() {
        if (showGraph) {
            jsonGraphScriptPanel.dispose();
        }
    }
}
