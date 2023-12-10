package my.lang.action;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;

import java.io.File;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;

/**
 * @author yanchangyou
 */
public class FitLangToolAction extends ScriptRunCodeAction {

    String name;

    String fitPath;

    protected FitLangToolAction(String name, String title, String fitPath) {
        super(title);
        this.name = name;
        this.fitPath = fitPath;
    }

    @Override
    public void actionPerformed(AnActionEvent e) {

        final Project project = e.getProject();

        initConsoleViewIfNeed(project, getLanguageName(), getLogoString(), getProjectConsoleViewMap());

        if (windowMap.get(project) != null) {
            windowMap.get(project).show(() -> {
            });
        }

        final Editor editor = e.getData(CommonDataKeys.EDITOR);
        VirtualFile[] virtualFiles = e.getData(PlatformDataKeys.VIRTUAL_FILE_ARRAY);

        VirtualFile virtualFile = e.getData(CommonDataKeys.VIRTUAL_FILE);

        if (editor != null && virtualFile != null) {
        } else if (virtualFiles != null && virtualFiles.length > 0) {
            virtualFile = virtualFiles[0];
        } else {
            return;
        }
        String filePath = virtualFile.getPath();

        VirtualFile finalVirtualFile = virtualFile;
        threadPoolExecutor.submit(() -> {

            String projectPath = e.getProject() == null ? "" : e.getProject().getBasePath();

            JSONObject contextParam = buildContextParam(projectPath, new File(filePath));

            String result = runFitFile(fitPath, contextParam);
            if (isJsonObjectText(result)) {
                result = JSON.parseObject(result).toJSONString(JSONWriter.Feature.PrettyFormat);
            }

            print(result + "\n\n", project, getProjectConsoleViewMap());

            finalVirtualFile.getParent().refresh(false, false);

        });
    }
}