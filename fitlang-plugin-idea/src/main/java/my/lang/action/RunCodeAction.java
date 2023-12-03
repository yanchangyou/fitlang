package my.lang.action;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IoUtil;
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
import fit.lang.plugin.json.function.JsonPackageExecuteNode;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;

import java.io.File;
import java.io.InputStream;
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
            if (!isCodeFile(virtualFile)) {
                print("This file can't execute!\n\n", project, getProjectConsoleViewMap());
                return;
            }

            filePathList.add(virtualFile.getPath());

        } else if (virtualFiles != null) {
            //多选时，数组长度大于1
            for (VirtualFile virtualFile1 : virtualFiles) {

                List<File> files = FileUtil.loopFiles(virtualFile1.getPath());
                for (File file : files) {
                    if (file.isDirectory()) {
                        continue;
                    }
                    if (isCodeFile(file.getName())) {
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

        if (filePathList.isEmpty()) {
            return;
        }

        boolean finalNeedShowFile = needShowFile;
        threadPoolExecutor.submit(() -> {

            for (String path : filePathList) {
                String code = readNodeDefineFile(path);

                boolean needFormatJsonInConsole = needFormatJsonInConsole(code);
                String result;
                try {

                    if (finalNeedShowFile) {
                        print("run file: " + path + "\n", project, getProjectConsoleViewMap());
                    }

                    Object resultObject = executeCode(code, path);

                    result = resultObject.toString();

                } catch (Throwable exception) {
                    exception.printStackTrace();
                    result = "exception:" + getAllException(exception);
                    print(result + "\n\n", project, getProjectConsoleViewMap());
                    // throw exception; //TODO
                }

                System.out.println("execute " + getLanguageName() + " code result:");
                System.out.println(result);

                print(result.concat("\n\n"), project, getProjectConsoleViewMap());

            }
        });
    }

    private static boolean needFormatJsonInConsole(String code) {
        return code.contains("\"_needFormatJsonInConsole\"") || code.contains("needFormatJsonInConsoleFlag");
    }

    String executeCode(String code, String codePath) {

        ServerJsonExecuteNode.setCurrentServerFilePath(codePath);
        JsonPackageExecuteNode.addImportPath(ServerJsonExecuteNode.getServerFileDir());

        File file = new File(codePath);
        if (!file.exists()) {
            return "file not existed: ".concat(codePath);
        }
        String fileName = file.getName();
        JSONObject contextParam = new JSONObject();
        contextParam.put("filePath", file.getAbsolutePath());
        contextParam.put("fileName", file.getName());
        contextParam.put("fileDir", file.getParent());
        contextParam.put("filePrefix", fileName.split("\\.")[0]);
        String fileSuffix = null;
        if (fileName.contains(".")) {
            fileSuffix = fileName.split("\\.")[1];
            contextParam.put("fileSuffix", fileSuffix);
        }

        boolean needFormatJsonInConsole = false;
        String result;
        if (isMyLanguageFile(file.getName())) {
            result = ExecuteJsonNodeUtil.executeCode(code, contextParam);
            needFormatJsonInConsole = needFormatJsonInConsole(code);
        } else if (fileSuffix != null && supportLanguageMap.containsKey(fileSuffix)) {
            result = runLanguageFile(fileSuffix, contextParam);
            needFormatJsonInConsole = true;
        } else {
            result = "can not execute: ".concat(codePath);
        }

        if (needFormatJsonInConsole && isJsonObjectText(result)) {
            result = JSONObject.parse(result).toJSONString(JSONWriter.Feature.PrettyFormat);
        }

        return result;
    }

    static JSONObject supportLanguageMap = new JSONObject();

    static String supportLanguageFileDir = "";

    static {
        supportLanguageMap.put("java", "");
        supportLanguageMap.put("go", "");
        supportLanguageMap.put("js", "");
        supportLanguageMap.put("rs", "");
        supportLanguageMap.put("py", "");
    }

    String runLanguageFile(String fileType, JSONObject contextParam) {
        String path = "fit/plugin/code/".concat(fileType).concat(".fit");
        InputStream in = RunCodeAction.class.getClassLoader().getResourceAsStream(path);
        if (in == null) {
            return "can not found: ".concat(path);
        }
        String fitCode = IoUtil.readUtf8(in);
        return ExecuteJsonNodeUtil.executeCode(fitCode, contextParam);
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

    boolean isCodeFile(VirtualFile virtualFile) {
        if (virtualFile == null) {
            return false;
        }
        return isCodeFile(virtualFile.getName());
    }

    boolean isCodeFile(String fileName) {
        if (fileName == null) {
            return false;
        }
        return fileName.contains(".");
    }
}
