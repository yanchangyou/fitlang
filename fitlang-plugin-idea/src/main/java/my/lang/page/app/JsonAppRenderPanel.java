package my.lang.page.app;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.components.JBScrollPane;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.parseJsonSchema;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;
import static my.lang.action.RunCodeAction.implementIdeOperator;

public class JsonAppRenderPanel extends JPanel {

    Project project;

    JSONObject appDefine;

    JSONObject contextParam;

    JSONObject scriptJson = JSONObject.parse("{'uni':'hello'}");

    VirtualFile appFile;

    JsonObjectEditorPanel inputEditor;

    JsonObjectEditorPanel outputEditor;

    JsonScriptEditorPanel scriptEditor;

    String appTitle = "App";
    String inputTitle = "Input";
    String outputTitle = "Output";
    String scriptTitle = "Script";
    String defaultButtonTitle = "Run";

    /**
     * 出入参结构不同，导致不能交换
     */
    boolean showExchangeButton;

    public JsonAppRenderPanel(@NotNull Project project, JSONObject appDefine, VirtualFile appFile, JSONObject contextParam) {

        this.project = project;
        this.appDefine = appDefine;
        this.appFile = appFile;
        this.contextParam = contextParam;

        appTitle = appDefine.containsKey("title") ? appDefine.getString("title") : appTitle;
        inputTitle = appDefine.containsKey("inputTitle") ? appDefine.getString("inputTitle") : inputTitle;
        outputTitle = appDefine.containsKey("outputTitle") ? appDefine.getString("outputTitle") : outputTitle;
        scriptTitle = appDefine.containsKey("scriptTitle") ? appDefine.getString("scriptTitle") : scriptTitle;
        defaultButtonTitle = appDefine.containsKey("defaultButtonTitle") ? appDefine.getString("defaultButtonTitle") : defaultButtonTitle;
        showExchangeButton = Boolean.TRUE.equals(appDefine.getBoolean("showExchangeButton"));

        JSONArray actions = appDefine.getJSONArray("actions");

        JSONObject input = appDefine.getJSONObject("input");
        JSONObject output = appDefine.getJSONObject("output");

        if (appDefine.containsKey("script")) {
            scriptJson = appDefine.getJSONObject("script");
        }

        if (input == null) {
            input = new JSONObject();
        }

        if (output == null) {
            output = new JSONObject();
        }

        setBorder(null);
        setLayout(new BorderLayout());

        setAppTitle(appTitle);

        JSplitPane splitPane = buildMainPanel(input, output, actions);
        splitPane.setBorder(null);
//        splitPane.setDividerSize(4);

        adjustSplitPanel(splitPane);

        implementIdeOperator(null);

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

        scriptEditor = new JsonScriptEditorPanel(scriptJson, scriptTitle, SwingConstants.LEFT, project);

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

                        String result = ExecuteJsonNodeUtil.executeCode(input, script, contextParam);

                        JSONObject output = JSONObject.parse(result);
                        setOutputJson(output);
                    }
                });
                toolBar.add(button);
            }
        }

        JBScrollPane jbScrollPane = new JBScrollPane(toolBar);

        panel.add(jbScrollPane, BorderLayout.NORTH);

        return panel;
    }

    @NotNull
    private JPanel buildToolBar() {
        JPanel toolBar = new JPanel();

        if (showExchangeButton) {

            //add switch Run Button
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

        //add default Run Button
        {
            JButton button = new JButton(defaultButtonTitle);
            button.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {

                    JSONObject script = getScriptJson();

                    JSONObject input = getInputJson();

                    String result = ExecuteJsonNodeUtil.executeCode(input, script, contextParam);

                    JSONObject output = JSONObject.parse(result);

                    setOutputJson(output);
                }
            });
            toolBar.add(button);
        }

        return toolBar;
    }

    private void setInputJson(JSONObject input) {
        inputEditor.getJsonFormEditor().setFormData(input);
        inputEditor.getJsonFormEditor().setFormDataToChrome(input);
        inputEditor.getJsonTextEditor().setText(toJsonTextWithFormat(input));
    }

    private JSONObject getOutputJson() {
        return outputEditor.getJsonObject();
    }

    private JSONObject getScriptJson() {
        return JSONObject.parse(scriptEditor.getJsonTextEditor().getText());
    }

    private void setOutputJson(JSONObject output) {
        outputEditor.getJsonFormEditor().setFormData(output);
        outputEditor.getJsonFormEditor().setFormDataToChrome(output);
        outputEditor.getJsonTextEditor().setText(toJsonTextWithFormat(output));
    }

    @NotNull
    private JSONObject getInputJson() {
        return inputEditor.getJsonObject();
    }

    private JComponent buildInputAndOutputObjectPanel(JSONObject input, JSONObject output) {

        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setDividerSize(3);
        splitPane.setBorder(null);

        inputEditor = new JsonObjectEditorPanel(parseJsonSchema(input), input, inputTitle, SwingConstants.LEFT, project);

        splitPane.add(inputEditor);

        outputEditor = new JsonObjectEditorPanel(parseJsonSchema(output), output, outputTitle, SwingConstants.RIGHT, project);

        splitPane.add(outputEditor);

        adjustSplitPanel(splitPane);

        return splitPane;

    }

    private static void adjustSplitPanel(JSplitPane splitPane) {
        new Thread(() -> {
            for (int i = 0; i < 10; i++) {
                try {
                    Thread.sleep(500L);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
                splitPane.setDividerLocation(0.5);
            }
        }).start();
    }

    public void dispose() {
        inputEditor.dispose();
        outputEditor.dispose();
        scriptEditor.dispose();
    }
}
