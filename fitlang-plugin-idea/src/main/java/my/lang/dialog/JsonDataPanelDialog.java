package my.lang.dialog;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.json.json5.Json5Language;
import com.intellij.openapi.editor.ex.util.EditorUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.ui.LanguageTextField;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

public class JsonDataPanelDialog extends DialogWrapper {

    JSONObject jsonData;

    JSONObject option;

    JSONObject context;

    Project project;

    public JsonDataPanelDialog(Project project, JSONObject jsonData, JSONObject option, JSONObject context) {

        super(true);
        this.project = project;
        this.jsonData = jsonData;
        if (option == null) {
            option = new JSONObject();
        }
        if (context == null) {
            context = new JSONObject();
        }

        init();

    }

    @Override
    protected @Nullable JComponent createCenterPanel() {
        LanguageTextField jsonTextEditor = new LanguageTextField(Json5Language.INSTANCE, project, toJsonTextWithFormat(jsonData));
        jsonTextEditor.setFont(EditorUtil.getEditorFont());
        jsonTextEditor.setOneLineMode(false);

        return jsonTextEditor;
    }
}
