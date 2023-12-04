package my.lang.action;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;

import java.util.Map;

import static my.lang.MyLanguage.LANG_NAME;
import static my.lang.MyLanguage.LANG_STRING_LOGO;

/**
 * @author yanchangyou
 */
public class ScriptRunCodeForZipAction extends RunCodeAction {

    @Override
    public String getLanguageName() {
        return LANG_NAME;
    }

    @Override
    public String getLogoString() {
        return LANG_STRING_LOGO;
    }

    @Override
    public Map<Project, ConsoleView> getProjectConsoleViewMap() {
        return projectConsoleViewMap;
    }

    @Override
    public void actionPerformed(AnActionEvent e) {

        final Project project = e.getProject();

        initConsoleViewIfNeed(project, getLanguageName(), getLogoString(), getProjectConsoleViewMap());

        if (windowMap.get(project) != null) {
            windowMap.get(project).show(() -> {
            });
        }

        String filePath;
        final Editor editor = e.getData(CommonDataKeys.EDITOR);
        VirtualFile[] virtualFiles = e.getData(PlatformDataKeys.VIRTUAL_FILE_ARRAY);

        if (editor != null) {
            VirtualFile virtualFile = e.getData(CommonDataKeys.VIRTUAL_FILE);
            filePath = virtualFile.getPath();
        } else if (virtualFiles != null) {
            filePath = virtualFiles[0].getPath();
        } else {
            return;
        }

        if (filePath == null) {
            return;
        }

        threadPoolExecutor.submit(() -> {

            JSONObject contextParam = new JSONObject();
            contextParam.put("filePath", filePath);

            String result = runLanguageFile("zip", contextParam);

            print(result, project, getProjectConsoleViewMap());

        });
    }
}