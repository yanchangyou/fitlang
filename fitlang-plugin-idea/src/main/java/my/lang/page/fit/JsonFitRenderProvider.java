package my.lang.page.fit;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorPolicy;
import com.intellij.openapi.fileEditor.FileEditorProvider;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class JsonFitRenderProvider implements FileEditorProvider, DumbAware {

    JsonFitRender jsonFitRender;

    JSONObject contextParam;

    public JsonFitRenderProvider() {
        this.contextParam = new JSONObject();
    }

    @Override
    public boolean accept(@NotNull Project project, @NotNull VirtualFile virtualFile) {
        return virtualFile.getName().endsWith(".fit");
    }

    @Override
    public @NotNull FileEditor createEditor(@NotNull Project project, @NotNull VirtualFile virtualFile) {
        jsonFitRender = new JsonFitRender(project, virtualFile, contextParam);
        return jsonFitRender;
    }

    @Override
    public @NotNull @NonNls String getEditorTypeId() {
        return ".fit";
    }

    @Override
    public @NotNull FileEditorPolicy getPolicy() {
        return FileEditorPolicy.PLACE_AFTER_DEFAULT_EDITOR;
    }

}
