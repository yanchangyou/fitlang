package my.lang.page.app;

import cn.hutool.core.io.IoUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.ComboBox;
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

    JSONArray actions;

    JComboBox<String> actionComBox;

    JsonScriptEditorPanel scriptEditor;

    JPanel buttonsPanel;

    String appTitle = "App";
    String inputTitle = "Input";
    String outputTitle = "Output";
    String scriptTitle = "Script";

    JLabel appTitleLabel;

    JButton exchangeButton;

    JButton reloadButton;

    JButton clearOutputButton;

    JButton executeButton;

    JButton saveButton;

    JButton compareButton;

    JButton openChromeDevButton;
    JButton switchViewButton;
    JButton switchFormButton;
    JButton resetLayoutButton;

    String reloadButtonTitle = "Reload";
    String clearOutputButtonTitle = "Clear Output";
    String executeButtonTitle = "Execute";
    String resetLayoutButtonTitle = "Layout";
    String needSortButtonTitle = "Sort";
    String compareButtonTitle = "Compare";
    String saveButtonTitle = "Save";
    String openChromeDevButtonTitle = "Open Chrome Dev";
    String switchViewButtonTitle = "Switch View";
    String switchFormButtonTitle = "Switch";

    /**
     * 是否使用图形界面，chrome有内存泄露问题
     */
    boolean showGraph;

    boolean showSwitchFormButton = true;

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

        init(appDefine);

        scriptSplitPane.setBorder(null);
//        splitPane.setDividerSize(4);

        adjustSplitPanel(scriptSplitPane);

        implementIdeOperator(null, project);

    }

    private void init(JSONObject appDefine) {
        JSONObject uiDefine = appDefine;
        if (appDefine.containsKey("ui")) {
            uiDefine = appDefine.getJSONObject("ui");
        }


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
        showSwitchFormButton = !hideButtons.contains("switchForm");

        actions = uiDefine.getJSONArray("actions");

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

        buildAppTitle(appTitle);

        setAppTitle(appTitle);

        scriptSplitPane = buildMainPanel(input, output, actions);

        resetAllTitle(uiDefine);

        resetAllButtonName(uiDefine);
    }

    private void resetAllTitle(JSONObject uiDefine) {

        appTitle = uiDefine.containsKey("title") ? uiDefine.getString("title") : appTitle;
        inputTitle = uiDefine.containsKey("inputTitle") ? uiDefine.getString("inputTitle") : inputTitle;
        outputTitle = uiDefine.containsKey("outputTitle") ? uiDefine.getString("outputTitle") : outputTitle;
        scriptTitle = uiDefine.containsKey("scriptTitle") ? uiDefine.getString("scriptTitle") : scriptTitle;

        setAppTitle(appTitle);
        inputEditor.setTitle(inputTitle);
        outputEditor.setTitle(outputTitle);
        scriptEditor.setTitle(scriptTitle);

    }

    private void resetAllButtonName(JSONObject uiDefine) {
        reloadButtonTitle = uiDefine.containsKey("reloadButtonTitle") ? uiDefine.getString("reloadButtonTitle") : reloadButtonTitle;
        clearOutputButtonTitle = uiDefine.containsKey("clearOutputButtonTitle") ? uiDefine.getString("clearOutputButtonTitle") : clearOutputButtonTitle;
        executeButtonTitle = uiDefine.containsKey("executeButtonTitle") ? uiDefine.getString("executeButtonTitle") : executeButtonTitle;
        resetLayoutButtonTitle = uiDefine.containsKey("resetLayoutButtonTitle") ? uiDefine.getString("resetLayoutButtonTitle") : resetLayoutButtonTitle;
        saveButtonTitle = uiDefine.containsKey("saveButtonTitle") ? uiDefine.getString("saveButtonTitle") : saveButtonTitle;
        compareButtonTitle = uiDefine.containsKey("compareButtonTitle") ? uiDefine.getString("compareButtonTitle") : compareButtonTitle;
        needSortButtonTitle = uiDefine.containsKey("needSortButtonTitle") ? uiDefine.getString("needSortButtonTitle") : needSortButtonTitle;
        openChromeDevButtonTitle = uiDefine.containsKey("openChromeDevButtonTitle") ? uiDefine.getString("openChromeDevButtonTitle") : openChromeDevButtonTitle;
        switchViewButtonTitle = uiDefine.containsKey("switchViewButtonTitle") ? uiDefine.getString("switchViewButtonTitle") : switchViewButtonTitle;
        switchFormButtonTitle = uiDefine.containsKey("switchFormButtonTitle") ? uiDefine.getString("switchFormButtonTitle") : switchFormButtonTitle;

        if (exchangeButton != null) exchangeButton.setText("<->");
        if (reloadButton != null) reloadButton.setText(reloadButtonTitle);
        if (clearOutputButton != null) clearOutputButton.setText(clearOutputButtonTitle);
        if (executeButton != null) executeButton.setText(executeButtonTitle);
        if (saveButton != null) saveButton.setText(saveButtonTitle);
        if (compareButton != null) compareButton.setText(compareButtonTitle);
        if (openChromeDevButton != null) openChromeDevButton.setText(openChromeDevButtonTitle);
        if (switchViewButton != null) switchViewButton.setText(switchViewButtonTitle);
        if (switchFormButton != null) switchFormButton.setText(switchFormButtonTitle);
        if (resetLayoutButton != null) resetLayoutButton.setText(resetLayoutButtonTitle);
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

    void setAppTitle(String title) {
        appTitleLabel.setText(title);
    }

    private void buildAppTitle(String title) {
        appTitleLabel = new JLabel(title, SwingConstants.CENTER);
        Font defaultFont = appTitleLabel.getFont();
        Font font = new Font(null, defaultFont.getStyle(), defaultFont.getSize() + 2);
        appTitleLabel.setFont(font);
        JPanel panel = new JPanel(new BorderLayout());
        panel.add(new JLabel(" "), BorderLayout.NORTH);
        panel.add(appTitleLabel, BorderLayout.CENTER);
        add(panel, BorderLayout.NORTH);
    }

    JPanel buildScriptPanel(JSONArray actions) {

        JPanel panel = new JPanel(new BorderLayout());

        JPanel toolBar = buildToolBar();

        scriptEditor = new JsonScriptEditorPanel(scriptDefine, scriptTitle, SwingConstants.LEFT, showGraph, project);

        panel.add(scriptEditor, BorderLayout.CENTER);

        buttonsPanel = new JPanel();
        toolBar.add(buttonsPanel);

        addActionButtons(actions);

        JBScrollPane jbScrollPane = new JBScrollPane(toolBar);
        Dimension dimension = new Dimension(toolBar.getWidth(), toolBar.getHeight() + 65);
        jbScrollPane.setPreferredSize(dimension);
        jbScrollPane.setBorder(JBUI.Borders.empty(5));
        panel.add(jbScrollPane, BorderLayout.NORTH);

        return panel;
    }

    private void addActionButtons(JSONArray actions) {

        for (Component component : buttonsPanel.getComponents()) {
            buttonsPanel.remove(component);
        }

        if (actions != null) {

            while (actionComBox.getItemCount() > 2) {
                actionComBox.removeItemAt(2);
            }

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
                buttonsPanel.add(button);

                actionComBox.addItem(title);

            }
        }
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
            resetLayoutButton = new JButton(resetLayoutButtonTitle);
            resetLayoutButton.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    adjustSplitPanel(scriptSplitPane);
                    adjustSplitPanel(inputOutputSplitPane);
                }
            });
            toolBar.add(resetLayoutButton);
        }

        //switchForm
        if (showGraph) {
            {
                switchViewButton = new JButton(switchViewButtonTitle);
                switchViewButton.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        inputEditor.switchView();
                        outputEditor.switchView();

                        // TODO BUG
                        scriptEditor.cardLayout.next(scriptEditor.cardPanel);

                    }
                });
                toolBar.add(switchViewButton);
            }
        }

        if (showGraph) {
            {
                openChromeDevButton = new JButton(openChromeDevButtonTitle);
                openChromeDevButton.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        scriptEditor.jsonGraphScriptPanel.openDevtools();

                    }
                });
                toolBar.add(openChromeDevButton);
            }
        }

        //reload
        if (showReloadButton) {

            //add reload Button
            {
                reloadButton = new JButton(reloadButtonTitle);
                reloadButton.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        JSONObject theAppDefine = JsonAppRender.readAppDefine(appFile.getPath());
//
                        JSONObject input = theAppDefine.getJSONObject("input");
                        JSONObject output = theAppDefine.getJSONObject("output");
                        JSONObject script = theAppDefine.getJSONObject("script");

                        setOutputJson(output);
                        setInputJson(input);

                        scriptEditor.getJsonTextEditor().setText(toJsonTextWithFormat(script));

                        JSONObject uiDefine = theAppDefine.getJSONObject("ui");
                        if (uiDefine != null) {
                            actions = uiDefine.getJSONArray("actions");
                            addActionButtons(actions);

                            appTitleLabel.requestFocus();

                            resetAllTitle(uiDefine);
                            resetAllButtonName(uiDefine);
                        }

                    }
                });
                toolBar.add(reloadButton);
            }
        }

        //add switch view Button
        if (showSwitchFormButton) {
            {
                switchFormButton = new JButton(switchFormButtonTitle);
                switchFormButton.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        inputEditor.switchView();
                        outputEditor.switchView();

                    }
                });
                toolBar.add(switchFormButton);
            }
        }

        if (showExchangeButton) {

            //add exchange Button
            {
                exchangeButton = new JButton("<->");
                exchangeButton.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        JSONObject input = getInputJson();

                        JSONObject output = getOutputJson();

                        setOutputJson(input);
                        setInputJson(output);

                    }
                });
                toolBar.add(exchangeButton);
            }
        }

        //add 比较 Button
        if (showCompareButton) {
            JCheckBox isNeedSort = new JCheckBox(needSortButtonTitle);
            toolBar.add(isNeedSort);

            compareButton = new JButton(compareButtonTitle);
            compareButton.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    scriptEditor.jsonDiffResultPanel.showDiff(inputEditor.getJsonObject(), outputEditor.getJsonObject(), isNeedSort.isSelected());
                    scriptEditor.cardLayout.last(scriptEditor.cardPanel);
                }
            });
            toolBar.add(compareButton);
        }

        if (showClearOutputButton) {

            //add clear output Button
            {
                clearOutputButton = new JButton(clearOutputButtonTitle);
                clearOutputButton.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        JSONObject output = new JSONObject();
                        setOutputJson(output);

                    }
                });
                toolBar.add(clearOutputButton);
            }
        }

        //下拉选择action
        {
            String[] actionList = new String[]{"Default", "New"};
            actionComBox = new ComboBox<>(actionList);

            actionComBox.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    JSONObject script = new JSONObject();
                    Object selected = actionComBox.getSelectedItem();

                    if ("Default".equals(selected)) {
                        script = scriptDefine;
                    } else if ("New".equals(selected)) {
                        //TODO
                    } else {
                        for (Object action : actions) {
                            JSONObject actionJson = ((JSONObject) action);
                            String title = actionJson.getString("title");
                            if (title.equals(selected)) {
                                script = actionJson.getJSONObject("script");
                            }
                        }
                    }

                    scriptEditor.getJsonTextEditor().setText(toJsonTextWithFormat(script));
                }
            });
            toolBar.add(actionComBox);
        }

        //add default execute Button
        if (showExecuteButton) {
            executeButton = new JButton(executeButtonTitle);
            executeButton.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {

                    JSONObject script = getScriptDefine();

                    JSONObject input = getInputJson();

                    execute(input, script);
                    scriptEditor.cardLayout.first(scriptEditor.cardPanel);

                }
            });
            toolBar.add(executeButton);
        }

        if (showSaveButton) {
            saveButton = new JButton(saveButtonTitle);
            saveButton.addActionListener(new AbstractAction() {
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
                                JSONObject script = getScriptDefine();
                                appletDefine.put("script", script);
                                String content = new String(IoUtil.readBytes(appFile.getInputStream()));
                                JSONObject rawAppletDefine = JSONObject.parse(content);
                                if ("applet".equals(rawAppletDefine.getString("uni"))) {
                                    rawAppletDefine.putAll(appletDefine);
                                } else {
                                    appletDefine.put("script", rawAppletDefine);
                                    rawAppletDefine.remove("input");
                                    rawAppletDefine = appletDefine;
                                }
                                Object selectItem = actionComBox.getSelectedItem();
                                if (rawAppletDefine.containsKey("ui")) {
                                    JSONObject ui = rawAppletDefine.getJSONObject("ui");
                                    appletDefine.put("ui", ui);

                                    //deal action
                                    JSONArray actions = ui.getJSONArray("actions");
                                    for (Object action : actions) {
                                        JSONObject actionJson = (JSONObject) action;
                                        String title = actionJson.getString("title");
                                        if (title.equals(selectItem)) {
                                            actionJson.put("script", script);
                                            break;
                                        }
                                    }
                                }
                                {

                                    if ("New".equals(selectItem)) {
                                        JSONObject ui = appletDefine.getJSONObject("ui");
                                        if (ui == null) {
                                            ui = new JSONObject();
                                            appletDefine.put("ui", ui);
                                        }
                                        JSONArray actions = ui.getJSONArray("actions");
                                        if (actions == null) {
                                            actions = new JSONArray();
                                            ui.put("actions", actions);
                                        }
                                        String newActionTitle = Messages.showInputDialog("New action name:", "Input", null);
                                        if (newActionTitle == null) {
                                            newActionTitle = "Action";
                                        }
                                        JSONObject newAction = new JSONObject();
                                        newAction.put("title", newActionTitle);
                                        newAction.put("script", script);
                                        actions.add(newAction);
                                    }
                                }
                                String newJsonText = toJsonTextWithFormat(appletDefine);
                                appFile.setBinaryContent(newJsonText.getBytes(StandardCharsets.UTF_8));
                                appFile.refresh(false, false);
                                ApplicationManager.getApplication().invokeLaterOnWriteThread(new Runnable() {
                                    @Override
                                    public void run() {
                                        Messages.showInfoMessage("Save OK!", "Info");
                                    }
                                });
                            } catch (IOException e) {
                                throw new RuntimeException(e);
                            }
                        }
                    });

                }
            });
            toolBar.add(saveButton);
        }

        return toolBar;
    }

    private void setInputJson(JSONObject input) {
        inputEditor.setJsonObject(input);
    }

    private JSONObject getOutputJson() {
        return outputEditor.getJsonObject();
    }

    private JSONObject getScriptDefine() {
        return JSONObject.parse(scriptEditor.getJsonTextEditor().getText());
    }

    private void setOutputJson(JSONObject output) {
        outputEditor.setJsonObject(output);
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
                if (splitPane.getOrientation() == JSplitPane.HORIZONTAL_SPLIT) {
                    splitPane.setDividerLocation(0.5);
                } else {
                    splitPane.setDividerLocation(0.4);
                }
            }
        }).start();
    }

    public void dispose() {
        inputEditor.dispose();
        outputEditor.dispose();
        scriptEditor.dispose();
    }
}
