package my.lang.page.render;

import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorPolicy;
import com.intellij.openapi.fileEditor.FileEditorProvider;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.Set;

public class JsonPageRenderProvider implements FileEditorProvider, DumbAware {

    static Set<String> fileTypeSet = new HashSet();

    static {
        registerFileType(".amis.json");
        registerFileType(".logicflow.json");
    }

    static void registerFileType(String fileType) {
        fileTypeSet.add(fileType);
    }

    JsonPageRender jsonPageRender;

    public JsonPageRenderProvider() {

    }

    @Override
    public boolean accept(@NotNull Project project, @NotNull VirtualFile virtualFile) {
        for (String fileType : fileTypeSet) {
            if (virtualFile.getName().endsWith(fileType)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public @NotNull FileEditor createEditor(@NotNull Project project, @NotNull VirtualFile virtualFile) {
        jsonPageRender = new JsonPageRender(virtualFile);
        return jsonPageRender;
    }

    @Override
    public @NotNull @NonNls String getEditorTypeId() {
        return ".json";
    }

    @Override
    public @NotNull FileEditorPolicy getPolicy() {
        return FileEditorPolicy.PLACE_BEFORE_DEFAULT_EDITOR;
    }

}
