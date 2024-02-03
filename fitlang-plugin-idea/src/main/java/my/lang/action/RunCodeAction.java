package my.lang.action;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IoUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.execution.ui.ConsoleViewContentType;
import com.intellij.find.SearchReplaceComponent;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileChooser.FileChooser;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.ui.content.Content;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.function.JsonPackageExecuteNode;
import fit.lang.plugin.json.ide.node.UserIdeInterface;
import fit.lang.plugin.json.ide.node.UserIdeManager;
import fit.lang.plugin.json.util.ExecuteNodeLogActionable;
import fit.lang.plugin.json.util.LogJsonExecuteNode;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;
import my.lang.dialog.*;

import java.awt.*;
import java.io.File;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.List;
import java.util.*;
import java.util.concurrent.*;

import static fit.lang.ExecuteNodeUtil.getRootException;
import static fit.lang.plugin.json.ExecuteJsonNodeConst.FIELD_NAME_OF_IDEA_PROJECT;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.*;
import static my.lang.MyLanguage.isFitLanguageFile;

/**
 * 插件父类
 *
 * @author yanchangyou
 * @date 2020-09-11 22:48:45
 */
public abstract class RunCodeAction extends AnAction {

    static final Map<Project, ConsoleView> projectConsoleViewMap = new ConcurrentHashMap<>();

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

    protected RunCodeAction() {
    }

    protected RunCodeAction(String title) {
        super(title);
    }

    /**
     * 获取 project到consoleView的映射
     *
     * @return
     */
    public abstract Map<Project, ConsoleView> getProjectConsoleViewMap();

    public static Map<Project, ToolWindow> windowMap = new HashMap<Project, ToolWindow>();

    private final BlockingQueue<Runnable> workQueue = new ArrayBlockingQueue<>(100);
    ThreadPoolExecutor threadPoolExecutor = new ThreadPoolExecutor(8, 100, 100, TimeUnit.MINUTES, workQueue);

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

        implementIdeOperator(e);

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

        if (filePathList.size() == 1 && (isSynchronize(filePathList.get(0)) || filePathList.get(0).contains(".amis.") || filePathList.get(0).contains(".xrender."))) {
            execute(e, project, filePathList, false);
        } else {
            threadPoolExecutor.submit(() -> {
                execute(e, project, filePathList, finalNeedShowFile);
            });
        }
    }

    private static boolean isSynchronize(String filePath) {
        String content = readNodeDefineFile(filePath);
        return content.contains("open") || content.contains("show") || content.contains("choose");
    }

    private void execute(AnActionEvent e, Project project, List<String> filePathList, boolean finalNeedShowFile) {
        for (String path : filePathList) {
            String code = readNodeDefineFile(path);

            String result;
            try {

                if (finalNeedShowFile) {
                    print("run file: " + path + "\n", project, getProjectConsoleViewMap());
                }

                String projectPath = e.getProject() == null ? "" : e.getProject().getBasePath();

                //支持中间输出
                LogJsonExecuteNode.setPrintable(new ExecuteNodeLogActionable() {
                    @Override
                    public void print(Object info) {
                        if (info == null) {
                            info = "";
                        }
                        String infoText;
                        if (info instanceof Map) {
                            infoText = toJsonTextWithFormat(JSONObject.from(info));
                        } else if (info instanceof Collection) {
                            infoText = JSON.toJSONString(info);
                        } else {
                            JSONObject jsonObject = new JSONObject();
                            jsonObject.put("_raw", info);
                            infoText = toJsonTextWithFormat(jsonObject);
                        }
                        RunCodeAction.print(infoText + "\n", project, getProjectConsoleViewMap());
                    }
                });
                Object resultObject = executeCode(code, path, projectPath, project);

                result = resultObject.toString();

            } catch (Throwable exception) {
                exception.printStackTrace();
                result = "exception:" + getRootException(exception);
            }

            System.out.println("execute " + getLanguageName() + " code result:");
            System.out.println(result);

            print(result.concat("\n\n"), project, getProjectConsoleViewMap());


        }
    }

    private static boolean needFormatJsonInConsole(String code) {
        return true; //默认IDE执行的全部格式化
    }

    String executeCode(String code, String codePath, String projectPath, Project project) {

        File file = new File(codePath);
        if (!file.exists()) {
            return "file not existed: ".concat(codePath);
        }

        ServerJsonExecuteNode.setCurrentServerFilePath(codePath);
        JsonPackageExecuteNode.addImportPath(ServerJsonExecuteNode.getServerFileDir());

        JSONObject contextParam = buildContextParam(projectPath, file);

        contextParam.put(FIELD_NAME_OF_IDEA_PROJECT, project);

        String fileName = file.getName();

        String fileSuffix = null;
        if (fileName.contains(".")) {
            fileSuffix = fileName.substring(fileName.indexOf(".") + 1);
            contextParam.put("fileSuffix", fileSuffix);
        }

        boolean needFormatJsonInConsole = false;
        String result;
        if (isFitLanguageFile(fileName)) {
            result = ExecuteJsonNodeUtil.executeCode(code, contextParam);
            needFormatJsonInConsole = needFormatJsonInConsole(code);
        } else if (fileSuffix != null && supportLanguageMap.containsKey(fileSuffix)) {

            result = runLanguageFile(fileSuffix, contextParam);

            needFormatJsonInConsole = true;
        } else {
            result = "can not execute: ".concat(codePath);
        }

        if (needFormatJsonInConsole && isJsonObjectText(result)) {
            result = toJsonTextWithFormat(JSONObject.parse(result));
        }

        return result;
    }

    static Map<String, JSONObject> supportLanguageMap = new HashMap<>();

    String runLanguageFile(String fileType, JSONObject contextParam) {

        JSONObject script = supportLanguageMap.get(fileType);

        String result = ExecuteJsonNodeUtil.executeCode(new JSONObject(), script, contextParam);
        if (isJsonObjectText(result)) {
            JSONObject resultJson = JSONObject.parseObject(result);
            JSONArray lines;
            if (resultJson.getJSONObject("result") != null) {
                lines = resultJson.getJSONObject("result").getJSONArray("out");
            } else if (resultJson.getJSONArray("list") != null) {
                JSONArray list = resultJson.getJSONArray("list");
                lines = list.getJSONObject(list.size() - 1).getJSONObject("result").getJSONArray("out");
            } else {
                return result;
            }
            return lines == null ? "" : StrUtil.join("\n", lines);
        }
        return "error: ".concat(result);
    }

    String runFitFile(String fitPath, JSONObject contextParam) {
        String fitCode = getPluginFile(fitPath);
        return ExecuteJsonNodeUtil.executeCode(fitCode, contextParam);
    }

    static String getPluginFile(String path) {
        if (path.startsWith("~")) {
            return FileUtil.readUtf8String(path);
        }
        if (path.startsWith("file:")) {
            return FileUtil.readUtf8String(path.substring("file:".length()));
        }
        InputStream in = RunCodeAction.class.getClassLoader().getResourceAsStream(path);
        if (in == null) {
            throw new RuntimeException("getPluginFile can not found: ".concat(path));
        }
        return IoUtil.readUtf8(in);
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
        toolWindow.getContentManager().setSelectedContent(content);
        consoleView.print(logoString + "\n", ConsoleViewContentType.NORMAL_OUTPUT);
    }

    static boolean isInitiated(Project project, String languageName, Map<Project, ConsoleView> projectConsoleViewMap) {
        return projectConsoleViewMap.get(project) != null;
    }

    static void print(String result, Project project, Map<Project, ConsoleView> projectConsoleViewMap) {
        projectConsoleViewMap.get(project).print(result, ConsoleViewContentType.NORMAL_OUTPUT);

        //低版本idea不支持此方法，兼容处理
        scrollToEnd(projectConsoleViewMap.get(project));
    }

    static void scrollToEnd(ConsoleView consoleView) {
        try {
            Method method = ConsoleView.class.getMethod("requestScrollingToEnd");
            method.invoke(consoleView);
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

    /**
     * IDE 操作实现
     *
     * @param actionEvent
     */
    public static void implementIdeOperator(AnActionEvent actionEvent) {
        /**
         *
         */
        UserIdeManager.setUserIdeInterface(new UserIdeInterface() {

            Editor getEditor() {
                return actionEvent.getData(CommonDataKeys.EDITOR);
            }

            SearchReplaceComponent getSearchReplaceComponent() {
                Component headerComponent = getEditor().getHeaderComponent();

                if (headerComponent instanceof SearchReplaceComponent) {
                    return (SearchReplaceComponent) headerComponent;
                }
                return null;
            }

            @Override
            public String readEditorContent() {
                return getEditor().getDocument().getText();
            }

            @Override
            public void writeEditorContent(String content) {

                WriteCommandAction.runWriteCommandAction(getEditor().getProject(), new Runnable() {
                    @Override
                    public void run() {
                        getEditor().getDocument().setText(content);
                    }
                });
            }

            public void openWebPage(String url, JSONObject option, JSONObject context) {

                WebPagePanelDialog webPagePanelDialog = new WebPagePanelDialog(url, option, context);
                webPagePanelDialog.show();

            }

            public void showHtml(String html, JSONObject option, JSONObject context) {

                HtmlPanelDialog htmlPanelDialog = new HtmlPanelDialog(html, option, context);
                htmlPanelDialog.show();

            }

            @Override
            public JSONObject showJsonPage(JSONObject jsonPage, JSONObject jsonData, JSONObject option, JSONObject context) {

                JsonPagePanelDialog jsonPageDialog = new JsonPagePanelDialog(jsonPage, jsonData, option, context);
                if (jsonPageDialog.isModal()) {
                    jsonPageDialog.showAndGet();
                } else {
                    jsonPageDialog.show();
                }

                return jsonPageDialog.getJsonData();
            }

            @Override
            public JSONObject showDiff(JSONObject json1, JSONObject json2, JSONObject option, JSONObject context) {

                DiffDialogWrapper diffDialogWrapper = new DiffDialogWrapper(getEditor().getProject(), json1, json2);
                diffDialogWrapper.show();

                return json1;
            }


            @Override
            public JSONObject showNodeConfig(JSONObject config, Project project) {

                NodeConfigAction.nodeConfigPanel.resetConfig(config, project);


                return NodeConfigAction.nodeConfigPanel.readConfig();
            }

            @Override
            public JSONObject showGlobalConfigDialog(JSONObject config, JSONObject option) {

                GlobalConfigPanelDialog globalConfigPanelDialog = new GlobalConfigPanelDialog(config, option);
                globalConfigPanelDialog.showAndGet();

                return globalConfigPanelDialog.readConfig();
            }


            @Override
            public JSONObject getNodeConfig() {
                return NodeConfigAction.nodeConfigPanel.readConfig();
            }

            @Override
            public List<File> chooseFiles(JSONObject config) {
                Editor editor = getEditor();
                Project project = editor.getProject();
                boolean chooseFiles = true;
                boolean chooseFolders = false;
                boolean chooseJars = false;
                boolean chooseJarsAsFiles = false;
                boolean chooseJarContents = false;
                boolean chooseMultiple = Boolean.TRUE.equals(config.getBoolean("isMultiple"));
                FileChooserDescriptor fileChooserDescriptor = new FileChooserDescriptor(chooseFiles,
                        chooseFolders,
                        chooseJars,
                        chooseJarsAsFiles,
                        chooseJarContents,
                        chooseMultiple);
                VirtualFile[] files = FileChooser.chooseFiles(fileChooserDescriptor, project, null);
                List<File> fileList = new ArrayList<>();
                for (VirtualFile file : files) {
                    fileList.add(new File(file.getPath()));
                }
                return fileList;
            }

            @Override
            public void showInfoMessage(String title, String message) {
                Messages.showInfoMessage(message, title);
            }

            @Override
            public String showPasswordDialog(String title, String message) {
                return Messages.showPasswordDialog(message, title);
            }

            @Override
            public int showCheckboxOkCancelDialog(String title, String message, String checkboxText) {
                return Messages.showCheckboxOkCancelDialog(message, title, checkboxText, false, 0, 0, null);
            }

            @Override
            public void showErrorDialog(String title, String message) {
                Messages.showErrorDialog(message, title);
            }

            @Override
            public String showInputDialog(String title, String message) {
                return Messages.showInputDialog(message, title, null);
            }

            @Override
            public int showOkCancelDialog(String title, String message, String okText, String cancelText) {
                return Messages.showOkCancelDialog(message, title, okText, cancelText, null);
            }

            @Override
            public int showYesNoCancelDialog(String title, String message) {
                return Messages.showYesNoCancelDialog(message, title, null);
            }

            @Override
            public void showWarningDialog(String title, String message) {
                Messages.showWarningDialog(message, title);
            }

        });
    }
}