package my.lang.action;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.execution.ui.ConsoleViewContentType;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.ui.content.Content;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;

import java.io.File;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import static fit.lang.ExecuteNodeUtil.getAllException;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.*;
import static my.lang.MyLanguage.isMyLanguageFile;

/**
 * 插件父类
 *
 * @author yanchangyou
 * @date 2020-09-11 22:48:45
 */
public abstract class RunCodeAction extends AnAction {

    /**
     * 获取id
     *
     * @return
     */
    public abstract String getLanguageName();

    /**
     * 添加logo字符串
     *
     * @return
     */
    public abstract String getLogoString();

    /**
     * 获取 project到consoleView的映射
     *
     * @return
     */
    public abstract Map<Project, ConsoleView> getProjectConsoleViewMap();

    public static Map<Project, ToolWindow> windowMap = new HashMap<Project, ToolWindow>();

    private final BlockingQueue<Runnable> workQueue = new ArrayBlockingQueue<>(100);
    ThreadPoolExecutor threadPoolExecutor = new ThreadPoolExecutor(4, 10, 10, TimeUnit.MINUTES, workQueue);

    @Override
    public void actionPerformed(AnActionEvent e) {

        final Project project = e.getProject();

        initConsoleViewIfNeed(project, getLanguageName(), getLogoString(), getProjectConsoleViewMap());

        if (windowMap.get(project) != null) {
            windowMap.get(project).show(() -> {
            });
        }

        List<String> filePathList = new ArrayList<>();

        final Editor editor = e.getData(CommonDataKeys.EDITOR);
        VirtualFile[] virtualFiles = e.getData(PlatformDataKeys.VIRTUAL_FILE_ARRAY);

        boolean needShowFile = false;

        if (editor != null) {
            VirtualFile virtualFile = e.getData(CommonDataKeys.VIRTUAL_FILE);
            //检查文件后缀名是否满足
            if (!isScriptCode(virtualFile)) {
                print("This file can't execute!\n\n", project, getProjectConsoleViewMap());
                return;
            }

            filePathList.add(virtualFile.getPath());

        } else if (virtualFiles != null) {
            //多选时，数组长度大于1
            for (VirtualFile virtualFile1 : virtualFiles) {

                List<File> files = FileUtil.loopFiles(virtualFile1.getPath());
                for (File file : files) {
                    if (isMyLanguageFile(file.getName())) {
                        filePathList.add(file.getPath());
                    } else {
                        //检查文件后缀名是否满足
                        print("This file can't execute, ignore: ".concat(file.getAbsolutePath()).concat("\n\n"), project, getProjectConsoleViewMap());
                    }
                }
            }
            if (filePathList.size() > 1) {
                needShowFile = true;
            }
        } else {
            return;
        }

        if (!filePathList.isEmpty()) {
            ServerJsonExecuteNode.setCurrentServerFilePath(filePathList.get(0));
        } else {
            return;
        }

        boolean finalNeedShowFile = needShowFile;
        threadPoolExecutor.submit(() -> {

            for (String path : filePathList) {
                String code = readNodeDefineFile(path);
                String result;
                boolean needFormatJsonInConsole = false;
                try {

                    if (finalNeedShowFile) {
                        print("run file: " + path + "\n", project, getProjectConsoleViewMap());
                    }
                    if (code.contains("_needFormatJsonInConsole")) {
                        needFormatJsonInConsole = true;
                    }

                    Object resultObject = executeCode(code);

                    result = resultObject.toString();

                } catch (Throwable exception) {
                    exception.printStackTrace();
                    result = "exception:" + getAllException(exception);
                    print(result + "\n\n", project, getProjectConsoleViewMap());
                    throw exception;
                }

                System.out.println("execute " + getLanguageName() + " code result:");
                System.out.println(result);

                if (needFormatJsonInConsole && isJsonObjectText(result)) {
                    result = JSONObject.parse(result).toJSONString(JSONWriter.Feature.PrettyFormat);
                }

                print(result.concat("\n\n"), project, getProjectConsoleViewMap());

            }
        });
    }

    private String executeCode(String code) {
        return ExecuteJsonNodeUtil.executeCode(code);
    }

    public static synchronized void initConsoleViewIfNeed(Project project, String languageName, String logoString, Map<Project, ConsoleView> projectConsoleViewMap) {
        if (isInitiated(project, languageName, projectConsoleViewMap)) {
            return;
        }

        System.out.println(logoString);

        ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow(languageName + " Console");
        ConsoleView consoleView = TextConsoleBuilderFactory.getInstance().createBuilder(project).getConsole();

        windowMap.put(project, toolWindow);

        projectConsoleViewMap.put(project, consoleView);

        consoleView.allowHeavyFilters();
        Content content = toolWindow.getContentManager().getFactory().createContent(consoleView.getComponent(), languageName, false);
        toolWindow.getContentManager().addContent(content);
        consoleView.print(logoString + "\n", ConsoleViewContentType.NORMAL_OUTPUT);
    }

    static boolean isInitiated(Project project, String languageName, Map<Project, ConsoleView> projectConsoleViewMap) {
        return projectConsoleViewMap.get(project) != null;
    }

    void print(String result, Project project, Map<Project, ConsoleView> projectConsoleViewMap) {
        projectConsoleViewMap.get(project).print(result, ConsoleViewContentType.NORMAL_OUTPUT);

        //低版本idea不支持此方法，兼容处理
        scrollToEnd(projectConsoleViewMap.get(project));
    }

    private void scrollToEnd(ConsoleView consoleView) {
        try {
            Method method = ConsoleView.class.getMethod("requestScrollingToEnd", new Class[0]);
            method.invoke(consoleView, new Object[0]);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    boolean isScriptCode(VirtualFile virtualFile) {
        if (virtualFile == null) {
            return false;
        }
        return isMyLanguageFile(virtualFile.getName());
    }

}
