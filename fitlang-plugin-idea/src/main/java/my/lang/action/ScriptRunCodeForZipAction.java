package my.lang.action;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;

/**
 * @author yanchangyou
 */
public class ScriptRunCodeForZipAction extends ScriptRunCodeAction {

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

        VirtualFile virtualFile = e.getData(CommonDataKeys.VIRTUAL_FILE);

        if (editor != null && virtualFile != null) {
            filePath = virtualFile.getPath();
        } else if (virtualFiles != null && virtualFiles.length > 0) {
            filePath = virtualFiles[0].getPath();
        } else {
            return;
        }

        threadPoolExecutor.submit(() -> {

            JSONObject contextParam = new JSONObject();
            contextParam.put("path", filePath);

            String result = runLanguageFile("zip", contextParam);

            print(result, project, getProjectConsoleViewMap());

        });
    }
}