package my.lang.page.fit;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorState;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFile;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import my.lang.dialog.JsonDataPanelDialog;
import my.lang.page.app.JsonGraphScriptPanel;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;
import static my.lang.action.RunCodeAction.implementIdeOperator;

public class JsonFitRender implements FileEditor {

    JsonGraphScriptPanel jsonGraphScriptPanel;

    VirtualFile file;
    JSONObject contextParam;
    Project project;
    JPanel mainPanel;

    public JsonFitRender(@NotNull Project project, @NotNull VirtualFile virtualFile, JSONObject contextParam) {
        this.project = project;
        file = virtualFile;
        this.contextParam = contextParam;
        String path = file.getPath();

        String content = FileUtil.readUtf8String(path);
        if (StrUtil.isBlank(content)) {
            content = "{}";
        }

        JSONObject fitDefineJson = JSONObject.parse(content);

        jsonGraphScriptPanel = new JsonGraphScriptPanel(fitDefineJson, null);
        mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(jsonGraphScriptPanel, BorderLayout.CENTER);

        JPanel toolBar = buildToolBar();

        mainPanel.add(toolBar, BorderLayout.NORTH);

        implementIdeOperator(null, project);

    }

    private JPanel buildToolBar() {
        JPanel toolBar = new JPanel();

        JButton runButton = new JButton("执行");
        runButton.addActionListener(new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                JSONObject script = jsonGraphScriptPanel.getScript();
                execute(new JSONObject(), script);

            }
        });
        toolBar.add(runButton);

        JButton button = new JButton("保存");
        button.addActionListener(new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                JSONObject script = jsonGraphScriptPanel.getScript();
                ApplicationManager.getApplication().runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        try {
                            String newJsonText = toJsonTextWithFormat(script);
                            file.setBinaryContent(newJsonText.getBytes(StandardCharsets.UTF_8));
                            file.refresh(false, false);
                            ApplicationManager.getApplication().invokeLaterOnWriteThread(new Runnable() {
                                @Override
                                public void run() {
                                    Messages.showInfoMessage("保存成功!", "Info");
                                }
                            });
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    }
                });

            }
        });
        toolBar.add(button);

        JButton debugButton = new JButton("打开Chrome Dev");
        debugButton.addActionListener(new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                jsonGraphScriptPanel.openDevtools();

            }
        });
        toolBar.add(debugButton);

//        JButton closeDebugButton = new JButton("关闭Chrome Dev");
//        closeDebugButton.addActionListener(new AbstractAction() {
//            @Override
//            public void actionPerformed(ActionEvent actionEvent) {
//
//                jsonGraphScriptPanel.closeDevtools();
//
//            }
//        });
//        toolBar.add(closeDebugButton);

        return toolBar;
    }

    @Override
    public @NotNull JComponent getComponent() {
        return mainPanel;
    }

    @Override
    public @Nullable JComponent getPreferredFocusedComponent() {
        return mainPanel;
    }

    @Override
    public @Nls(capitalization = Nls.Capitalization.Title) @NotNull String getName() {
        return "Fit";
    }

    @Override
    public void setState(@NotNull FileEditorState fileEditorState) {
    }

    @Override
    public boolean isModified() {
        return false;
    }

    @Override
    public boolean isValid() {
        return true;
    }

    @Override
    public void addPropertyChangeListener(@NotNull PropertyChangeListener propertyChangeListener) {
    }

    @Override
    public void removePropertyChangeListener(@NotNull PropertyChangeListener propertyChangeListener) {

    }

    @Override
    public void dispose() {
        jsonGraphScriptPanel.dispose();
    }

    @Override
    public <T> @Nullable T getUserData(@NotNull Key<T> key) {
        return null;
    }

    @Override
    public <T> void putUserData(@NotNull Key<T> key, @Nullable T t) {
    }

    @Override
    public @NotNull VirtualFile getFile() {
        return file;
    }

    void execute(JSONObject input, JSONObject script) {
        try {
            String result = ExecuteJsonNodeUtil.executeCode(input, script, contextParam);
            JSONObject output = JSONObject.parse(result);

            JsonDataPanelDialog jsonDataPanelDialog = new JsonDataPanelDialog(project, output, null, null);
            jsonDataPanelDialog.setModal(true);
            jsonDataPanelDialog.showAndGet();
            jsonDataPanelDialog.disposeIfNeeded();

            System.out.println(output);
        } catch (Exception e) {
            Messages.showErrorDialog("ERROR: " + e.getLocalizedMessage(), "Error");
        }
    }

}
