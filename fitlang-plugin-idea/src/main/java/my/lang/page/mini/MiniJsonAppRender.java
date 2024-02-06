package my.lang.page.mini;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import my.lang.page.app.JsonAppRender;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

public class MiniJsonAppRender extends JsonAppRender {

    public MiniJsonAppRender(@NotNull Project project, @NotNull VirtualFile virtualFile, JSONObject contextParam) {
        super(project, virtualFile, contextParam);
    }

    @Override
    public @Nls(capitalization = Nls.Capitalization.Title) @NotNull String getName() {
        return "MiniApp";
    }

}
