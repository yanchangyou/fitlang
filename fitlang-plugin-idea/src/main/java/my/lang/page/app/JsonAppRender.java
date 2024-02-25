package my.lang.page.app;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorState;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.beans.PropertyChangeListener;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.readNodeDefineFile;

public class JsonAppRender implements FileEditor {

    JsonAppRenderPanel panel;

    VirtualFile file;
    JSONObject contextParam;
    Project project;

    public JsonAppRender(@NotNull Project project, @NotNull VirtualFile virtualFile, JSONObject contextParam) {
        this.project = project;
        file = virtualFile;
        this.contextParam = contextParam;
        String path = file.getPath();

        JSONObject appDefineJson = readAppDefine(path);

        panel = new JsonAppRenderPanel(project, appDefineJson, file, contextParam);
    }

    @NotNull
    public static JSONObject readAppDefine(String path) {
        String content = readNodeDefineFile(path);
        if (StrUtil.isBlank(content)) {
            content = "{'uni':'hello'}";
        }

        JSONObject appDefineJson = JSONObject.parse(content);

        //兼容无script的场景
        if (!appDefineJson.containsKey("script")) {
            JSONObject scriptJson = appDefineJson;
            appDefineJson = new JSONObject();
            appDefineJson.put("script", scriptJson);

            //兼容input字段
            if (scriptJson.containsKey("input")) {
                appDefineJson.put("input", scriptJson.remove("input"));
            }
        }
        if (!appDefineJson.containsKey("input")) {
            appDefineJson.put("input", new JSONObject());
        }
        if (!appDefineJson.containsKey("output")) {
            appDefineJson.put("output", new JSONObject());
        }
        return appDefineJson;
    }

    @Override
    public @NotNull JComponent getComponent() {
        return panel;
    }

    @Override
    public @Nullable JComponent getPreferredFocusedComponent() {
        return panel;
    }

    @Override
    public @Nls(capitalization = Nls.Capitalization.Title) @NotNull String getName() {
        return "JsonApp";
    }

    @Override
    public void setState(@NotNull FileEditorState fileEditorState) {
    }

    @Override
    public boolean isModified() {
        return false;
    }

    @Override
    public boolean isValid() {
        return true;
    }

    @Override
    public void addPropertyChangeListener(@NotNull PropertyChangeListener propertyChangeListener) {
    }

    @Override
    public void removePropertyChangeListener(@NotNull PropertyChangeListener propertyChangeListener) {

    }

    @Override
    public void dispose() {
        panel.dispose();
    }

    @Override
    public <T> @Nullable T getUserData(@NotNull Key<T> key) {
        return null;
    }

    @Override
    public <T> void putUserData(@NotNull Key<T> key, @Nullable T t) {
    }

    @Override
    public @NotNull VirtualFile getFile() {
        return file;
    }

}
