package my.lang.page.fit;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorState;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFile;
import my.lang.page.app.JsonGraphScriptPanel;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.beans.PropertyChangeListener;

public class JsonFitRender implements FileEditor {

    JsonGraphScriptPanel jsonGraphScriptPanel;

    VirtualFile file;
    JSONObject contextParam;
    Project project;

    public JsonFitRender(@NotNull Project project, @NotNull VirtualFile virtualFile, JSONObject contextParam) {
        this.project = project;
        file = virtualFile;
        this.contextParam = contextParam;
        String path = file.getPath();

        String content = FileUtil.readUtf8String(path);
        if (StrUtil.isBlank(content)) {
            content = "{}";
        }

        JSONObject fitDefineJson = JSONObject.parse(content);

        jsonGraphScriptPanel = new JsonGraphScriptPanel(fitDefineJson, null);
    }

    @Override
    public @NotNull JComponent getComponent() {
        return jsonGraphScriptPanel;
    }

    @Override
    public @Nullable JComponent getPreferredFocusedComponent() {
        return jsonGraphScriptPanel;
    }

    @Override
    public @Nls(capitalization = Nls.Capitalization.Title) @NotNull String getName() {
        return "Fit";
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
        jsonGraphScriptPanel.dispose();
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