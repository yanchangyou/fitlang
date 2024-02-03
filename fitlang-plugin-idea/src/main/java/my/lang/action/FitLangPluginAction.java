package my.lang.action;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;

import java.io.File;
import java.util.concurrent.ThreadPoolExecutor;

import static fit.lang.ExecuteNodeUtil.getRootException;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.*;
import static my.lang.MyLanguage.LANG_NAME;
import static my.lang.MyLanguage.LANG_STRING_LOGO;

/**
 * @author yanchangyou
 */
public class FitLangPluginAction extends ScriptRunCodeAction {

    PluginActionConfig actionConfig;

    protected FitLangPluginAction(JSONObject config) {
        super(config.getOrDefault("title", config.getString("name")).toString());

        actionConfig = new PluginActionConfig(config);
//
        //兼容问题; TODO
//        if (StrUtil.isNotBlank(actionConfig.shortCut1)) {
//            KeyStroke firstKeyStroke = KeyStroke.getKeyStroke(actionConfig.shortCut1);
//            KeyStroke secondKeyStroke = null;
//            if (StrUtil.isNotBlank(actionConfig.shortCut2)) {
//                secondKeyStroke = KeyStroke.getKeyStroke(actionConfig.shortCut2);
//            }
//            KeyboardShortcut keyboardShortcut = new KeyboardShortcut(firstKeyStroke, secondKeyStroke);
//            setShortcutSet(new CustomShortcutSet(keyboardShortcut));
//        }
    }

    /**
     * 注册action
     *
     * @param actionId
     * @param action
     */
    public static void registerAction(String actionId, AnAction action) {
        ActionManager actionManager = ActionManager.getInstance();
        if (actionManager.getAction(actionId) == null) {
            try {
                actionManager.registerAction(actionId, action, PluginId.getId("FitLang"));
            } catch (Exception e) {
                //ignore todo
            }
        }
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        actionPerformedInner(e, threadPoolExecutor, actionConfig);
    }

    static void actionPerformedInner(AnActionEvent e, ThreadPoolExecutor threadPoolExecutor, PluginActionConfig actionConfig) {

        final Project project = e.getProject();

        initConsoleViewIfNeed(project, LANG_NAME, LANG_STRING_LOGO, projectConsoleViewMap);

        if (windowMap.get(project) != null) {
            windowMap.get(project).show(() -> {
            });
        }

        final Editor editor = e.getData(CommonDataKeys.EDITOR);

        implementIdeOperator(e);

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

        if (actionConfig.isSynchronize()) {
            execute(e, actionConfig, project, filePath, finalVirtualFile);
        } else {
            threadPoolExecutor.submit(() -> {
                execute(e, actionConfig, project, filePath, finalVirtualFile);
            });
        }
    }

    private static void execute(AnActionEvent e, PluginActionConfig actionConfig, Project project, String filePath, VirtualFile finalVirtualFile) {
        String result;
        try {

            String projectPath = e.getProject() == null ? "" : e.getProject().getBasePath();

            JSONObject contextParam = buildContextParam(projectPath, new File(filePath));

            ServerJsonExecuteNode.setCurrentServerFilePath(filePath);

            result = ExecuteJsonNodeUtil.executeCode(null, actionConfig.getScript(), contextParam);

            if (isJsonObjectText(result)) {
                JSONObject jsonObject = JSON.parseObject(result);
                if (jsonObject.get("_raw") != null) {
                    Object raw = jsonObject.get("_raw");
                    if (raw instanceof JSONArray) {
                        result = toJsonTextWithFormat((JSONArray) raw);
                    } else {
                        result = raw.toString();
                    }
                } else {
                    result = toJsonTextWithFormat(jsonObject);
                }
            }

            print(result + "\n\n", project, projectConsoleViewMap);

            if (actionConfig.isRefreshParent()) {
                finalVirtualFile.getParent().refresh(false, false);
            }
            if (actionConfig.isRefresh()) {
                finalVirtualFile.refresh(false, false);
            }

        } catch (Exception exception) {
            exception.printStackTrace();
            result = "exception:" + getRootException(exception);
            print(result + "\n\n", project, projectConsoleViewMap);
        }
    }
}