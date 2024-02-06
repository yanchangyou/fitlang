package my.lang.page.mini;

import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import my.lang.page.app.JsonAppRenderProvider;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class MiniJsonAppRenderProvider extends JsonAppRenderProvider {

    MiniJsonAppRender miniJsonAppRender;

    @Override
    public @NotNull FileEditor createEditor(@NotNull Project project, @NotNull VirtualFile virtualFile) {
        miniJsonAppRender = new MiniJsonAppRender(project, virtualFile, contextParam);
        return miniJsonAppRender;
    }

    @Override
    public boolean accept(@NotNull Project project, @NotNull VirtualFile virtualFile) {
        return virtualFile.getName().endsWith(".fit");
    }


    @Override
    public @NotNull @NonNls String getEditorTypeId() {
        return ".fit";
    }

}
