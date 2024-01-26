package my.lang.page.render;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorState;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.beans.PropertyChangeListener;

public class JsonPageRender implements FileEditor {

    JsonPageRenderPanel panel;

    VirtualFile file;

    String type = "amis";

    public JsonPageRender(@NotNull VirtualFile virtualFile) {
        file = virtualFile;
        String path = file.getPath();

        String content = FileUtil.readUtf8String(path);

        JSONObject jsonPage = JSONObject.parse(content);

        if (jsonPage.containsKey("type")) {
            type = jsonPage.getString("type");
        }
        panel = new JsonPageRenderPanel(jsonPage);
    }

    String parseType(String name) {
        String type = "amis";
        if (name.indexOf(".") != name.lastIndexOf(".")) {
            type = name.substring(name.indexOf(".") + 1, name.lastIndexOf("."));
        }
        return type;
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
        return "JsonPage";
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
//        propertyChangeListener.propertyChange(new PropertyChangeEvent());
    }

    @Override
    public void removePropertyChangeListener(@NotNull PropertyChangeListener propertyChangeListener) {

    }

    @Override
    public void dispose() {
        panel.close();
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