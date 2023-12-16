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
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;

import java.io.File;
import java.util.concurrent.ThreadPoolExecutor;

import static fit.lang.ExecuteNodeUtil.getRootException;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;
import static my.lang.MyLanguage.LANG_NAME;
import static my.lang.MyLanguage.LANG_STRING_LOGO;

/**
 * @author yanchangyou
 */
public class FitLangPluginAction extends ScriptRunCodeAction {

    String name;

    JSONObject script;

    protected FitLangPluginAction(String name, String title, JSONObject script) {
        super(title);
        this.name = name;
        this.script = script;
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        actionPerformedInner(e, threadPoolExecutor, script);
    }

    static void actionPerformedInner(AnActionEvent e, ThreadPoolExecutor threadPoolExecutor, JSONObject actionScript) {

        final Project project = e.getProject();

        initConsoleViewIfNeed(project, LANG_NAME, LANG_STRING_LOGO, projectConsoleViewMap);

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
            String result;
            try {

                String projectPath = e.getProject() == null ? "" : e.getProject().getBasePath();

                JSONObject contextParam = buildContextParam(projectPath, new File(filePath));

                ServerJsonExecuteNode.setCurrentServerFilePath(filePath);

                result = ExecuteJsonNodeUtil.executeCode(null, actionScript, contextParam);

                if (isJsonObjectText(result)) {
                    result = JSON.parseObject(result).toJSONString(JSONWriter.Feature.PrettyFormat);
                }

                print(result + "\n\n", project, projectConsoleViewMap);

                finalVirtualFile.getParent().refresh(false, false);

            } catch (Exception exception) {
                exception.printStackTrace();
                result = "exception:" + getRootException(exception);
                print(result + "\n\n", project, projectConsoleViewMap);
            }
        });
    }
}