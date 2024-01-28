package my.lang.page.app;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorPolicy;
import com.intellij.openapi.fileEditor.FileEditorProvider;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class JsonAppRenderProvider implements FileEditorProvider, DumbAware {

    JsonAppRender jsonAppRender;

    JSONObject contextParam;

    public JsonAppRenderProvider() {
        this.contextParam = new JSONObject();
    }

    @Override
    public boolean accept(@NotNull Project project, @NotNull VirtualFile virtualFile) {
        return virtualFile.getName().endsWith(".app.json");
    }

    @Override
    public @NotNull FileEditor createEditor(@NotNull Project project, @NotNull VirtualFile virtualFile) {
        jsonAppRender = new JsonAppRender(project, virtualFile, contextParam);
        return jsonAppRender;
    }

    @Override
    public @NotNull @NonNls String getEditorTypeId() {
        return ".app.json";
    }

    @Override
    public @NotNull FileEditorPolicy getPolicy() {
        return FileEditorPolicy.PLACE_BEFORE_DEFAULT_EDITOR;
    }

}
