package my.lang.page.diff;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorPolicy;
import com.intellij.openapi.fileEditor.FileEditorProvider;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class JsonDiffRenderProvider implements FileEditorProvider, DumbAware {

    JsonDiffRender jsonDiffRender;

    JSONObject contextParam;

    public JsonDiffRenderProvider() {
        this.contextParam = new JSONObject();
    }

    @Override
    public boolean accept(@NotNull Project project, @NotNull VirtualFile virtualFile) {
        return virtualFile.getName().endsWith(".diff.json");
    }

    @Override
    public @NotNull FileEditor createEditor(@NotNull Project project, @NotNull VirtualFile virtualFile) {
        jsonDiffRender = new JsonDiffRender(project, virtualFile, contextParam);
        return jsonDiffRender;
    }

    @Override
    public @NotNull @NonNls String getEditorTypeId() {
        return ".diff.json";
    }

    @Override
    public @NotNull FileEditorPolicy getPolicy() {
        return FileEditorPolicy.PLACE_BEFORE_DEFAULT_EDITOR;
    }

}
