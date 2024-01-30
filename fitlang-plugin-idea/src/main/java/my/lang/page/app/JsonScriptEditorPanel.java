package my.lang.page.app;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;

public class JsonScriptEditorPanel extends JsonBaseEditorPanel {

    JsonGraphScriptPanel jsonGraphScriptPanel;

    JSONObject defaultLogicFlow = JSONObject.parseObject("{}");

    public JsonScriptEditorPanel(JSONObject script, String title, int horizontalAlignment, Project project) {

        super(script, title, horizontalAlignment, project);

        jsonGraphScriptPanel = new JsonGraphScriptPanel(defaultLogicFlow);
        cardPanel.add(jsonTextEditor);

        cardLayout.next(cardPanel);

    }
}
