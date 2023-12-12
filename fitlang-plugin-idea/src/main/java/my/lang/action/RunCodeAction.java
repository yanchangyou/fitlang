package my.lang.action;

import cn.hutool.core.io.CharsetDetector;
import cn.hutool.core.io.FileUtil;
import cn.hutool.core.io.IoUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.system.SystemUtil;
import com.alibaba.fastjson2.JSONArray;
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
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.*;

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

                String result;
                try {

                    if (finalNeedShowFile) {
                        print("run file: " + path + "\n", project, getProjectConsoleViewMap());
                    }

                    String projectPath = e.getProject() == null ? "" : e.getProject().getBasePath();

                    Object resultObject = executeCode(code, path, projectPath);

                    result = resultObject.toString();

                } catch (Throwable exception) {
                    exception.printStackTrace();
                    result = "exception:" + getAllException(exception);
                }

                System.out.println("execute " + getLanguageName() + " code result:");
                System.out.println(result);

                print(result.concat("\n\n"), project, getProjectConsoleViewMap());

            }
        });
    }

    private static boolean needFormatJsonInConsole(String code) {
        return code.contains("\"_needFormatJsonInConsole\"")
                || code.contains("needFormatJsonInConsoleFlag")
                || code.contains("GET http")
                || code.contains("POST http")
                || code.contains("PUT http")
                || code.contains("HEAD http")
                || code.contains("DELETE http")
                ;
    }

    String executeCode(String code, String codePath, String projectPath) {

        ServerJsonExecuteNode.setCurrentServerFilePath(codePath);
        JsonPackageExecuteNode.addImportPath(ServerJsonExecuteNode.getServerFileDir());

        File file = new File(codePath);
        if (!file.exists()) {
            return "file not existed: ".concat(codePath);
        }
        JSONObject contextParam = buildContextParam(projectPath, file);

        String fileName = file.getName();

        String fileSuffix = null;
        if (fileName.contains(".")) {
            fileSuffix = fileName.split("\\.")[1];
            contextParam.put("fileSuffix", fileSuffix);
        }

        boolean needFormatJsonInConsole = false;
        String result;
        if (isMyLanguageFile(fileName)) {
            result = ExecuteJsonNodeUtil.executeCode(code, contextParam);
            needFormatJsonInConsole = needFormatJsonInConsole(code);
        } else if (fileSuffix != null && supportLanguageMap.containsKey(fileSuffix)) {
            Charset charset = CharsetDetector.detect(file, characters);
            contextParam.put("charset", charset.name());
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

    static Charset[] characters = new Charset[]{
            StandardCharsets.UTF_8,
            Charset.forName("ISO8859-1"),
            StandardCharsets.UTF_8,
    };

    static {
        try {
            characters[2] = Charset.forName("GBK");
        } catch (Exception e) {
            //ignore
        }
    }

    static JSONObject buildContextParam(String projectPath, File file) {
        JSONObject contextParam = new JSONObject();

        contextParam.put("projectPath", projectPath);

        contextParam.put("fileDir", file.getParent());
        String fileDirInProject = file.getParent().substring(projectPath.length());
        contextParam.put("fileDirInProject", fileDirInProject);
        contextParam.put("fileDirInProject1", "");
        contextParam.put("fileDirInProject2", "");
        contextParam.put("fileDirInProject3", "");
        if (fileDirInProject.indexOf(File.separatorChar, 1) > 0) {
            String fileDirInProject1 = fileDirInProject.substring(fileDirInProject.indexOf(File.separatorChar, 1));
            contextParam.put("fileDirInProject1", fileDirInProject1);
            if (fileDirInProject1.indexOf(File.separatorChar, 1) > 0) {
                String fileDirInProject2 = fileDirInProject1.substring(fileDirInProject1.indexOf(File.separatorChar, 1));
                contextParam.put("fileDirInProject2", fileDirInProject2);
                if (fileDirInProject2.indexOf(File.separatorChar, 1) > 0) {
                    contextParam.put("fileDirInProject3", fileDirInProject2.substring(fileDirInProject2.indexOf(File.separatorChar, 1)));
                }
            }
        }

        contextParam.put("filePath", file.getAbsolutePath());
        contextParam.put("path", file.getAbsolutePath());
        contextParam.put("fileName", file.getName());
        contextParam.put("filePrefix", file.getName().split("\\.")[0]);
        if (file.getName().contains(".")) {
            contextParam.put("fileSuffix", file.getName().split("\\.")[1]);
        }
        contextParam.put("userHome", SystemUtil.getProps().get("user.home"));
        return contextParam;
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
        String fitPath = "fit/plugin/code/".concat(fileType).concat(".fit");
        String result = runFitFile(fitPath, contextParam);
        if (isJsonObjectText(result)) {
            JSONObject resultJson = JSONObject.parseObject(result);
            JSONArray lines;
            if (resultJson.containsKey("result")) {
                lines = resultJson.getJSONObject("result").getJSONArray("out");
            } else {
                JSONArray list = resultJson.getJSONArray("list");
                lines = list.getJSONObject(list.size() - 1).getJSONObject("result").getJSONArray("out");
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
