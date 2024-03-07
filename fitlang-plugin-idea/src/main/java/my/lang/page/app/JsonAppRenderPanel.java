package my.lang.page.app;

import cn.hutool.core.io.IoUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.ComboBox;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.Gray;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.util.ui.JBUI;
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

    /**
     * 是否异步执行
     */
    boolean isSynchronized;

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

    JButton formatButton;

    JButton reloadButton;

    JButton clearOutputButton;

    JButton executeButton;

    JButton saveButton;

    JButton compareButton;
    JCheckBox compareSortCheckBox;
    JButton openChromeDevButton;
    JButton switchViewButton;
    JButton switchFormButton;
    JButton resetLayoutButton;

    String reloadButtonTitle = "Refresh";
    String clearOutputButtonTitle = "Clear Output";
    String executeButtonTitle = "Execute";
    String resetLayoutButtonTitle = "Layout";
    String needSortButtonTitle = "Sort";
    String compareButtonTitle = "Compare";
    String saveButtonTitle = "Save";
    String openChromeDevButtonTitle = "Open Chrome Dev";
    String switchViewButtonTitle = "Switch View";
    String switchFormButtonTitle = "Switch";
    String formatButtonTitle = "Format";

    /**
     * 是否使用图形界面，chrome有内存泄露问题
     */
    boolean showGraph;

    boolean showSwitchFormButton = true;

    boolean showFormatButton = true;

    /**
     * 出入参结构不同，导致不能交换
     */
    boolean showExchangeButton = true;

    boolean showReloadButton = true;

    boolean showClearOutputButton = true;

    boolean showActionList = true;

    boolean showExecuteButton = true;

    boolean showSaveButton = true;

    boolean showCompareButton = true;

    boolean showInputForm = false;

    boolean showOutputForm = false;

    JSplitPane inputOutputSplitPane;

    JSplitPane scriptSplitPane;

    private Double inputOutputSplitRatio = 0.5;

    private Double scriptSplitRatio = 0.5;

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

        implementIdeOperator(null, project);
    }

    private void init(JSONObject appDefine) {
        JSONObject uiDefine = appDefine;

        isSynchronized = Boolean.TRUE.equals(appDefine.getBoolean("synchronize"));

        if (appDefine.containsKey("ui")) {
            uiDefine = appDefine.getJSONObject("ui");
        }

        showGraph = Boolean.TRUE.equals(uiDefine.getBoolean("showGraph"));

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
        scriptSplitPane.setDividerLocation(scriptSplitRatio);

        resetAllTitle(uiDefine);
        resetView(uiDefine);
        resetAllButtonName(uiDefine);
        restAllButtonVisible(uiDefine);
        readLayoutConfig(uiDefine);
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

    private void resetView(JSONObject uiDefine) {
        showInputForm = Boolean.TRUE.equals(uiDefine.getBoolean("showInputForm"));
        showOutputForm = Boolean.TRUE.equals(uiDefine.getBoolean("showOutputForm"));

        inputEditor.initView(showInputForm);
        outputEditor.initView(showOutputForm);
    }

    private void readLayoutConfig(JSONObject uiDefine) {
        if (uiDefine != null) {
            inputOutputSplitRatio = uiDefine.containsKey("inputOutputSplitRatio") ? uiDefine.getDouble("inputOutputSplitRatio") : inputOutputSplitRatio;
            scriptSplitRatio = uiDefine.containsKey("scriptSplitRatio") ? uiDefine.getDouble("scriptSplitRatio") : scriptSplitRatio;
        }

        adjustSplitPanel(inputOutputSplitPane, inputOutputSplitRatio);
        adjustSplitPanel(scriptSplitPane, scriptSplitRatio);
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
        formatButtonTitle = uiDefine.containsKey("formatButtonTitle") ? uiDefine.getString("formatButtonTitle") : formatButtonTitle;

        if (exchangeButton != null) exchangeButton.setText("<->");
        if (reloadButton != null) reloadButton.setText(reloadButtonTitle);
        if (clearOutputButton != null) clearOutputButton.setText(clearOutputButtonTitle);
        if (executeButton != null) executeButton.setText(executeButtonTitle);
        if (saveButton != null) saveButton.setText(saveButtonTitle);
        if (compareButton != null) compareButton.setText(compareButtonTitle);
        if (compareSortCheckBox != null) compareSortCheckBox.setText(needSortButtonTitle);
        if (openChromeDevButton != null) openChromeDevButton.setText(openChromeDevButtonTitle);
        if (switchViewButton != null) switchViewButton.setText(switchViewButtonTitle);
        if (switchFormButton != null) switchFormButton.setText(switchFormButtonTitle);
        if (formatButton != null) formatButton.setText(formatButtonTitle);
        if (resetLayoutButton != null) resetLayoutButton.setText(resetLayoutButtonTitle);
    }

    private void restAllButtonVisible(JSONObject uiDefine) {

        JSONArray hideButtons = uiDefine.getJSONArray("hideButtons");
        if (hideButtons == null) {
            hideButtons = new JSONArray();
        }

        showReloadButton = !hideButtons.contains("reload");
        showActionList = !hideButtons.contains("actionList");
        showClearOutputButton = !hideButtons.contains("clearOutput");
        showExchangeButton = !hideButtons.contains("exchange");
        showSaveButton = !hideButtons.contains("save");
        showExecuteButton = !hideButtons.contains("execute");
        showCompareButton = !hideButtons.contains("compare");
        showSwitchFormButton = !hideButtons.contains("switchForm");
        showFormatButton = !hideButtons.contains("format");

        actionComBox.setVisible(showActionList);
        saveButton.setVisible(showSaveButton);
        exchangeButton.setVisible(showExchangeButton);
        clearOutputButton.setVisible(showClearOutputButton);
        executeButton.setVisible(showExecuteButton);
        saveButton.setVisible(showSaveButton);
        compareButton.setVisible(showCompareButton);
        compareSortCheckBox.setVisible(showCompareButton);
        openChromeDevButton.setVisible(showGraph);
        switchViewButton.setVisible(showGraph);
        switchFormButton.setVisible(showSwitchFormButton);
        formatButton.setVisible(showFormatButton);
    }

    @NotNull
    private JSplitPane buildMainPanel(JSONObject input, JSONObject output, JSONArray actions) {
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        add(splitPane, BorderLayout.CENTER);
        splitPane.setDividerLocation(scriptSplitRatio);

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
            //异步执行
            if (isSynchronized) {
                new Thread(new Runnable() {
                    @Override
                    public void run() {

                        WriteCommandAction.runWriteCommandAction(project, () -> {
                            String result = executeCode(input, script, contextParam);
                            JSONObject output = JSONObject.parse(result);
                            setOutputJson(output);
                        });
                    }
                }).start();
            } else {
                String result = executeCode(input, script, contextParam);
                JSONObject output = JSONObject.parse(result);
                setOutputJson(output);
            }

        } catch (Exception e) {
            Messages.showErrorDialog("ERROR: " + e.getLocalizedMessage(), "Error");
        }
    }

    @NotNull
    private JPanel buildToolBar() {
        JPanel toolBar = new JPanel();

        //switchForm
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

        //add reload Button
        {
            reloadButton = new JButton(reloadButtonTitle);
            reloadButton.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    JSONObject theAppDefine = JsonAppRender.readAppDefine(appFile.getPath());

                    isSynchronized = Boolean.TRUE.equals(theAppDefine.getBoolean("synchronize"));

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
                        resetView(uiDefine);
                        resetAllButtonName(uiDefine);
                        restAllButtonVisible(uiDefine);
                    }

                    readLayoutConfig(uiDefine);
                }
            });
            toolBar.add(reloadButton);
        }

        //add switch view Button
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

        //add format Button
        formatButton = new JButton(formatButtonTitle);
        formatButton.addActionListener(new AbstractAction() {
            boolean format = true;

            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                JSONObject input = getInputJson();
                JSONObject output = getOutputJson();
                format = !format;
                setInputJson(input, format);
                setOutputJson(output, format);
            }
        });
        toolBar.add(formatButton);

        //add exchange Button
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

        //add 比较 Button
        {
            compareSortCheckBox = new JCheckBox(needSortButtonTitle);
            toolBar.add(compareSortCheckBox);

            compareButton = new JButton(compareButtonTitle);
            compareButton.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    scriptEditor.jsonDiffResultPanel.showDiff(inputEditor.getJsonObject(), outputEditor.getJsonObject(), compareSortCheckBox.isSelected());
                    scriptEditor.cardLayout.last(scriptEditor.cardPanel);
                }
            });
            toolBar.add(compareButton);
        }

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

        //下拉选择action
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
                    script = scriptDefine;
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

        //add default execute Button
        executeButton = new JButton(executeButtonTitle);
        executeButton.addActionListener(new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                JSONObject script = getScriptDefine();

                JSONObject input = JsonNativeFormPanel.parseRealFormData(getInputJson());

                execute(input, script);
                scriptEditor.cardLayout.first(scriptEditor.cardPanel);
            }
        });
        toolBar.add(executeButton);

        saveButton = new JButton(saveButtonTitle);
        saveButton.addActionListener(new AbstractAction() {

            @Override
            public void actionPerformed(ActionEvent actionEvent) {

                Object selectItem = actionComBox.getSelectedItem();
                String newActionTitle = "";
                if ("New".equals(selectItem)) {
                    newActionTitle = Messages.showInputDialog("New action name:", "Input", null);
                    if (StrUtil.isBlank(newActionTitle)) {
                        return;
                    }
                }

                String finalNewActionTitle = newActionTitle;
                ApplicationManager.getApplication().runWriteAction(() -> {
                    try {
                        JSONObject appletDefine = new JSONObject();
                        appletDefine.put("uni", "applet");
                        appletDefine.put("input", getInputJson());
                        appletDefine.put("output", getOutputJson());
                        JSONObject script = getScriptDefine();
                        appletDefine.put("script", script);
                        String content = new String(IoUtil.readBytes(appFile.getInputStream()));
                        if (StrUtil.isBlank(content)) {
                            content = "{'uni':'hello'}";
                        }
                        JSONObject rawAppletDefine = JSONObject.parse(content);
                        if ("applet".equals(rawAppletDefine.getString("uni"))) {
                            rawAppletDefine.putAll(appletDefine);
                        } else {
                            appletDefine.put("script", rawAppletDefine);
                            rawAppletDefine.remove("input");
                            rawAppletDefine = appletDefine;
                        }
                        if (!rawAppletDefine.containsKey("ui")) {
                            rawAppletDefine.put("ui", JSONObject.parse("{'hideButtons':['clearOutput','compare']}"));
                        }
                        JSONObject ui = rawAppletDefine.getJSONObject("ui");
                        appletDefine.put("ui", ui);

                        //deal action
                        JSONArray actions = ui.getJSONArray("actions");
                        if (actions == null) {
                            actions = new JSONArray();
                            ui.put("actions", actions);
                        }
                        for (Object action : actions) {
                            JSONObject actionJson = (JSONObject) action;
                            String title = actionJson.getString("title");
                            if (title.equals(selectItem)) {
                                actionJson.put("script", script);
                                break;
                            }
                        }

                        ui.put("scriptSplitRatio", Math.round(scriptSplitPane.getDividerLocation() * 100.0 / scriptSplitPane.getHeight()) / 100.0);
                        ui.put("inputOutputSplitRatio", Math.round(inputOutputSplitPane.getDividerLocation() * 100.0 / inputOutputSplitPane.getWidth()) / 100.0);

                        if (!ui.containsKey("hideButtons")) {
                            ui.put("hideButtons", JSONArray.parse("['clearOutput','compare']"));
                        }
                        if ("New".equals(selectItem)) {
                            JSONObject newAction = new JSONObject();
                            newAction.put("title", finalNewActionTitle);
                            newAction.put("script", script);
                            actions.add(newAction);
                        }
                        String newJsonText = toJsonTextWithFormat(appletDefine);
                        appFile.setBinaryContent(newJsonText.getBytes(StandardCharsets.UTF_8));
                        appFile.refresh(false, false);
                        ApplicationManager.getApplication().invokeLaterOnWriteThread(() -> Messages.showInfoMessage("Save OK!", "Info"));
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                });
            }
        });
        toolBar.add(saveButton);

        return toolBar;
    }

    private void setInputJson(JSONObject input) {
        setInputJson(input, true);
    }

    private void setInputJson(JSONObject input, boolean format) {
        inputEditor.setJsonObject(input, format);
    }

    private JSONObject getOutputJson() {
        return outputEditor.getJsonObject();
    }

    private JSONObject getScriptDefine() {
        return JSONObject.parse(scriptEditor.getJsonTextEditor().getText());
    }

    private void setOutputJson(JSONObject output) {
        setOutputJson(output, true);
    }

    private void setOutputJson(JSONObject output, boolean format) {
        outputEditor.setJsonObject(output, format);
    }

    @NotNull
    private JSONObject getInputJson() {
        return inputEditor.getJsonObject();
    }

    private JComponent buildInputAndOutputObjectPanel(JSONObject input, JSONObject output) {

        inputOutputSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
//        inputOutputSplitPane.setDividerSize(3);
        inputOutputSplitPane.setDividerLocation(inputOutputSplitRatio);
        inputOutputSplitPane.setBackground(Gray._250);

        inputOutputSplitPane.setBorder(null);

        inputEditor = new JsonObjectEditorPanel(inputForm, input, inputTitle, SwingConstants.LEFT, showGraph, project);

        inputOutputSplitPane.add(inputEditor);

        outputEditor = new JsonObjectEditorPanel(outputForm, output, outputTitle, SwingConstants.RIGHT, showGraph, project);

        inputOutputSplitPane.add(outputEditor);

        return inputOutputSplitPane;
    }

    private static void adjustSplitPanel(JSplitPane splitPane, double splitRatio) {
        new Thread(() -> {
            for (int i = 0; i < 4; i++) {
                try {
                    Thread.sleep(500L);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
                splitPane.setDividerLocation(splitRatio);
            }
        }).start();
    }

    public void dispose() {
        inputEditor.dispose();
        outputEditor.dispose();
        scriptEditor.dispose();
    }
}
