package my.lang.page.app;

import cn.hutool.core.io.IoUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.util.ui.JBUI;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import fit.lang.plugin.json.function.JsonPackageExecuteNode;
import fit.lang.plugin.json.web.ServerJsonExecuteNode;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.*;
import static my.lang.action.RunCodeAction.implementIdeOperator;

public class JsonAppRenderPanel extends JPanel {

    Project project;

    JSONObject appDefine;

    JSONObject contextParam;

    JSONObject scriptDefine = JSONObject.parse("{'uni':'hello'}");

    VirtualFile appFile;

    JsonObjectEditorPanel inputEditor;

    JsonObjectEditorPanel outputEditor;

    JSONObject inputForm;

    JSONObject outputForm;

    JsonScriptEditorPanel scriptEditor;

    String appTitle = "App";
    String inputTitle = "Input";
    String outputTitle = "Output";
    String scriptTitle = "Script";

    String reloadButtonTitle = "Reload";
    String clearOutputButtonTitle = "Clear Output";
    String executeButtonTitle = "Execute";
    String resetLayoutButtonTitle = "Layout";
    String needSortButtonTitle = "Sort";
    String compareButtonTitle = "Compare";
    String saveButtonTitle = "Save";
    String openChromeDevButtonTitle = "Open Chrome Dev";
    String switchViewButtonTitle = "Switch View";

    /**
     * 是否使用图形界面，chrome有内存泄露问题
     */
    boolean showGraph;

    /**
     * 出入参结构不同，导致不能交换
     */
    boolean showExchangeButton = true;

    boolean showReloadButton = true;

    boolean showClearOutputButton = true;

    boolean showExecuteButton = true;

    boolean showSaveButton = true;

    boolean showCompareButton = true;

    JSplitPane inputOutputSplitPane;

    JSplitPane scriptSplitPane;

    public JsonAppRenderPanel(@NotNull Project project, JSONObject appDefine, VirtualFile appFile, JSONObject contextParam) {

        if (contextParam == null) {
            contextParam = new JSONObject();
        }

        this.project = project;
        this.appDefine = appDefine;
        this.appFile = appFile;
        this.contextParam = contextParam;

        JSONObject uiDefine = appDefine;
        if (appDefine.containsKey("ui")) {
            uiDefine = appDefine.getJSONObject("ui");
        }

        appTitle = uiDefine.containsKey("title") ? uiDefine.getString("title") : appTitle;
        inputTitle = uiDefine.containsKey("inputTitle") ? uiDefine.getString("inputTitle") : inputTitle;
        outputTitle = uiDefine.containsKey("outputTitle") ? uiDefine.getString("outputTitle") : outputTitle;
        scriptTitle = uiDefine.containsKey("scriptTitle") ? uiDefine.getString("scriptTitle") : scriptTitle;

        reloadButtonTitle = uiDefine.containsKey("reloadButtonTitle") ? uiDefine.getString("reloadButtonTitle") : reloadButtonTitle;
        clearOutputButtonTitle = uiDefine.containsKey("clearOutputButtonTitle") ? uiDefine.getString("clearOutputButtonTitle") : clearOutputButtonTitle;
        executeButtonTitle = uiDefine.containsKey("executeButtonTitle") ? uiDefine.getString("executeButtonTitle") : executeButtonTitle;
        resetLayoutButtonTitle = uiDefine.containsKey("resetLayoutButtonTitle") ? uiDefine.getString("resetLayoutButtonTitle") : resetLayoutButtonTitle;
        saveButtonTitle = uiDefine.containsKey("saveButtonTitle") ? uiDefine.getString("saveButtonTitle") : saveButtonTitle;
        compareButtonTitle = uiDefine.containsKey("compareButtonTitle") ? uiDefine.getString("compareButtonTitle") : compareButtonTitle;
        needSortButtonTitle = uiDefine.containsKey("needSortButtonTitle") ? uiDefine.getString("needSortButtonTitle") : needSortButtonTitle;
        openChromeDevButtonTitle = uiDefine.containsKey("openChromeDevButtonTitle") ? uiDefine.getString("openChromeDevButtonTitle") : openChromeDevButtonTitle;
        switchViewButtonTitle = uiDefine.containsKey("switchViewButtonTitle") ? uiDefine.getString("switchViewButtonTitle") : switchViewButtonTitle;

        showGraph = Boolean.TRUE.equals(uiDefine.getBoolean("showGraph"));

        JSONArray hideButtons = uiDefine.getJSONArray("hideButtons");
        if (hideButtons == null) {
            hideButtons = new JSONArray();
        }

        showReloadButton = !hideButtons.contains("reload");
        showClearOutputButton = !hideButtons.contains("clearOutput");
        showExchangeButton = !hideButtons.contains("exchange");
        showSaveButton = !hideButtons.contains("save");
        showExecuteButton = !hideButtons.contains("execute");
        showCompareButton = !hideButtons.contains("compare");

        JSONArray actions = uiDefine.getJSONArray("actions");

        inputForm = uiDefine.getJSONObject("inputForm");
        outputForm = uiDefine.getJSONObject("outputForm");

        if (inputForm == null) {
            inputForm = new JSONObject();
        }

        if (outputForm == null) {
            outputForm = new JSONObject();
        }

        JSONObject input = appDefine.getJSONObject("input");
        JSONObject output = appDefine.getJSONObject("output");

        if (appDefine.containsKey("script")) {
            scriptDefine = appDefine.getJSONObject("script");
        }

        if (input == null) {
            input = new JSONObject();
        }

        if (output == null) {
            output = new JSONObject();
        }

        inputForm.put("schema", parseJsonSchema(input));
        outputForm.put("schema", parseJsonSchema(output));

        setBorder(null);
        setLayout(new BorderLayout());

        setAppTitle(appTitle);

        scriptSplitPane = buildMainPanel(input, output, actions);
        scriptSplitPane.setBorder(null);
//        splitPane.setDividerSize(4);

        adjustSplitPanel(scriptSplitPane);

        implementIdeOperator(null, project);

    }

    @NotNull
    private JSplitPane buildMainPanel(JSONObject input, JSONObject output, JSONArray actions) {
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        add(splitPane, BorderLayout.CENTER);

        JComponent inputOutputEditor = buildInputAndOutputObjectPanel(input, output);

        splitPane.add(inputOutputEditor);

        JComponent scriptPanel = buildScriptPanel(actions);

        splitPane.add(scriptPanel);

        return splitPane;
    }

    private void setAppTitle(String title) {
        JLabel titleLabel = new JLabel(title, SwingConstants.CENTER);
        Font defaultFont = titleLabel.getFont();
        Font font = new Font(null, defaultFont.getStyle(), defaultFont.getSize() + 2);
        titleLabel.setFont(font);
        JPanel panel = new JPanel(new BorderLayout());
        panel.add(new JLabel(" "), BorderLayout.NORTH);
        panel.add(titleLabel, BorderLayout.CENTER);
        add(panel, BorderLayout.NORTH);
    }

    JPanel buildScriptPanel(JSONArray actions) {

        JPanel panel = new JPanel(new BorderLayout());

        JPanel toolBar = buildToolBar();

        scriptEditor = new JsonScriptEditorPanel(scriptDefine, scriptTitle, SwingConstants.LEFT, showGraph, project);

        panel.add(scriptEditor, BorderLayout.CENTER);

        if (actions != null) {

            for (int i = 0; i < actions.size(); i++) {

                JSONObject action = actions.getJSONObject(i);
                String title = action.getString("title");
                JSONObject script = action.getJSONObject("script");

                JButton button = new JButton(title);
                button.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        scriptEditor.getJsonTextEditor().setText(toJsonTextWithFormat(script));
                        JSONObject input = getInputJson();
                        execute(input, script);
                    }
                });
                toolBar.add(button);
            }
        }

        JBScrollPane jbScrollPane = new JBScrollPane(toolBar);
        Dimension dimension = new Dimension(toolBar.getWidth(), toolBar.getHeight() + 65);
        jbScrollPane.setPreferredSize(dimension);
        jbScrollPane.setBorder(JBUI.Borders.empty(5));
        panel.add(jbScrollPane, BorderLayout.NORTH);

        return panel;
    }

    private void execute(JSONObject input, JSONObject script) {
        try {

            implementIdeOperator(null, project);

            ServerJsonExecuteNode.setCurrentServerFilePath(appFile.getPath());
            JsonPackageExecuteNode.addImportPath(ServerJsonExecuteNode.getServerFileDir());

            JSONObject newContextParam = buildContextParam(project.getBasePath(), new File(appFile.getPath()));
            contextParam.putAll(newContextParam);
            String result = ExecuteJsonNodeUtil.executeCode(input, script, contextParam);

            JSONObject output = JSONObject.parse(result);
            setOutputJson(output);
        } catch (Exception e) {
            Messages.showErrorDialog("ERROR: " + e.getLocalizedMessage(), "Error");
        }
    }

    @NotNull
    private JPanel buildToolBar() {
        JPanel toolBar = new JPanel();

        //reset layout
        {
            JButton button = new JButton(resetLayoutButtonTitle);
            button.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    adjustSplitPanel(scriptSplitPane);
                    adjustSplitPanel(inputOutputSplitPane);
                }
            });
            toolBar.add(button);
        }

        //add switch view Button
        if (showGraph) {
            {
                JButton button = new JButton(switchViewButtonTitle);
                button.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        inputEditor.cardLayout.next(inputEditor.cardPanel);
                        outputEditor.cardLayout.next(outputEditor.cardPanel);

                        // TODO BUG
                        scriptEditor.cardLayout.next(scriptEditor.cardPanel);

                    }
                });
                toolBar.add(button);
            }
        }

        if (showGraph) {
            {
                JButton debugButton = new JButton(openChromeDevButtonTitle);
                debugButton.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        scriptEditor.jsonGraphScriptPanel.openDevtools();

                    }
                });
                toolBar.add(debugButton);
            }
        }

        //reload
        if (showReloadButton) {

            //add reload Button
            {
                JButton button = new JButton(reloadButtonTitle);
                button.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        JSONObject theAppDefine = JsonAppRender.readAppDefine(appFile.getPath());

                        JSONObject input = theAppDefine.getJSONObject("input");
                        JSONObject output = theAppDefine.getJSONObject("output");
                        JSONObject script = theAppDefine.getJSONObject("script");

                        setOutputJson(output);
                        setInputJson(input);

                        scriptEditor.getJsonTextEditor().setText(toJsonTextWithFormat(script));

                    }
                });
                toolBar.add(button);
            }
        }

        if (showExchangeButton) {

            //add exchange Button
            {
                JButton button = new JButton("<->");
                button.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        JSONObject input = getInputJson();

                        JSONObject output = getOutputJson();

                        setOutputJson(input);
                        setInputJson(output);

                    }
                });
                toolBar.add(button);
            }
        }

        //add 比较 Button
        if (showCompareButton) {
            JCheckBox isNeedSort = new JCheckBox(needSortButtonTitle);
            toolBar.add(isNeedSort);

            JButton button = new JButton(compareButtonTitle);
            button.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {

                    scriptEditor.jsonDiffResultPanel.showDiff(inputEditor.getJsonObject(), outputEditor.getJsonObject(), isNeedSort.isSelected());
                    scriptEditor.cardLayout.last(scriptEditor.cardPanel);
                }
            });
            toolBar.add(button);
        }

        if (showClearOutputButton) {

            //add clear output Button
            {
                JButton button = new JButton(clearOutputButtonTitle);
                button.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        JSONObject output = new JSONObject();
                        setOutputJson(output);

                    }
                });
                toolBar.add(button);
            }
        }

        //add default execute Button
        if (showExecuteButton) {
            JButton button = new JButton(executeButtonTitle);
            button.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {

                    JSONObject script = getScriptDefine();

                    JSONObject input = getInputJson();

                    execute(input, script);
                    scriptEditor.cardLayout.first(scriptEditor.cardPanel);

                }
            });
            toolBar.add(button);
        }

        if (showSaveButton) {
            JButton button = new JButton(saveButtonTitle);
            button.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {

                    ApplicationManager.getApplication().runWriteAction(new Runnable() {
                        @Override
                        public void run() {
                            try {
                                JSONObject appletDefine = new JSONObject();
                                appletDefine.put("uni", "applet");
                                appletDefine.put("input", getInputJson());
                                appletDefine.put("output", getOutputJson());
                                appletDefine.put("script", getScriptDefine());
                                String content = new String(IoUtil.readBytes(appFile.getInputStream()));
                                JSONObject rawAppletDefine = JSONObject.parse(content);
                                if ("applet".equals(rawAppletDefine.getString("uni"))) {
                                    rawAppletDefine.putAll(appletDefine);
                                } else {
                                    appletDefine.put("script", rawAppletDefine);
                                    rawAppletDefine.remove("input");
                                    rawAppletDefine = appletDefine;
                                }
                                if (rawAppletDefine.containsKey("ui")) {
                                    appletDefine.put("ui", rawAppletDefine.getJSONObject("ui"));
                                }
                                String newJsonText = toJsonTextWithFormat(appletDefine);
                                appFile.setBinaryContent(newJsonText.getBytes(StandardCharsets.UTF_8));
                                appFile.refresh(false, false);
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
        }

        return toolBar;
    }

    private void setInputJson(JSONObject input) {
        if (showGraph) {
            inputEditor.getJsonFormEditor().setFormData(input);
            inputEditor.getJsonFormEditor().setFormDataToChrome(input);
        }
        inputEditor.getJsonTextEditor().setText(toJsonTextWithFormat(input));
    }

    private JSONObject getOutputJson() {
        return outputEditor.getJsonObject();
    }

    private JSONObject getScriptDefine() {
        return JSONObject.parse(scriptEditor.getJsonTextEditor().getText());
    }

    private void setOutputJson(JSONObject output) {
        if (showGraph) {
            outputEditor.getJsonFormEditor().setFormData(output);
            outputEditor.getJsonFormEditor().setFormDataToChrome(output);
        }
        outputEditor.getJsonTextEditor().setText(toJsonTextWithFormat(output));
    }

    @NotNull
    private JSONObject getInputJson() {
        return inputEditor.getJsonObject();
    }

    private JComponent buildInputAndOutputObjectPanel(JSONObject input, JSONObject output) {

        inputOutputSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        inputOutputSplitPane.setDividerSize(3);
        inputOutputSplitPane.setBorder(null);

        inputEditor = new JsonObjectEditorPanel(inputForm, input, inputTitle, SwingConstants.LEFT, showGraph, project);

        inputOutputSplitPane.add(inputEditor);

        outputEditor = new JsonObjectEditorPanel(outputForm, output, outputTitle, SwingConstants.RIGHT, showGraph, project);

        inputOutputSplitPane.add(outputEditor);

        adjustSplitPanel(inputOutputSplitPane);

        return inputOutputSplitPane;

    }

    private static void adjustSplitPanel(JSplitPane splitPane) {
        new Thread(() -> {
            for (int i = 0; i < 4; i++) {
                try {
                    Thread.sleep(500L);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
                splitPane.setDividerLocation(0.46);
            }
        }).start();
    }

    public void dispose() {
        inputEditor.dispose();
        outputEditor.dispose();
        scriptEditor.dispose();
    }
}
