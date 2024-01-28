package my.lang.page.app;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.json.json5.Json5Language;
import com.intellij.openapi.editor.ex.util.EditorUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.LanguageTextField;
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

    LanguageTextField inputEditor;
    LanguageTextField outputEditor;
    LanguageTextField scriptEditor;

    /**
     * 使用表单
     */
    boolean useForm;

    /**
     * 使用图形脚本
     */
    boolean useGraphScript;

    JsonFormPanel inputForm;
    JsonFormPanel outputForm;

    String appTitle = "App";
    String inputTitle = "Input";
    String outputTitle = "Output";
    String scriptTitle = "Script";
    String defaultButtonTitle = "Run";

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

        JSONArray actions = appDefine.getJSONArray("actions");
        useForm = Boolean.TRUE.equals(appDefine.getBoolean("useForm"));
        useGraphScript = Boolean.TRUE.equals(appDefine.getBoolean("useGraphScript"));

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

        JComponent inputOutputEditor;
        if (useForm) {
            inputOutputEditor = buildInputAndOutputForm(input, output);
        } else {
            inputOutputEditor = buildInputAndOutputEditor(input, output);
        }

        splitPane.add(inputOutputEditor);

        JComponent scriptPanel;
        if (useGraphScript) {
            scriptPanel = buildGraphScriptPanel(scriptJson);
        } else {
            scriptPanel = buildScriptPanel(actions);
        }

        splitPane.add(scriptPanel);

        return splitPane;
    }

    private void setAppTitle(String title) {
        JLabel titleLabel = new JLabel(title, SwingConstants.CENTER);
        JPanel panel = new JPanel(new BorderLayout());
        panel.add(new JLabel(" "), BorderLayout.NORTH);
        panel.add(titleLabel, BorderLayout.CENTER);
        add(panel, BorderLayout.NORTH);
    }

    JPanel buildGraphScriptPanel(JSONObject script) {

        JPanel panel = new JPanel(new BorderLayout());

        JPanel toolBar = buildToolBar();
        JsonGraphScriptPanel jsonGraphScriptPanel = new JsonGraphScriptPanel(script);

        panel.add(toolBar, BorderLayout.NORTH);
        panel.add(jsonGraphScriptPanel, BorderLayout.CENTER);

        return panel;
    }

    JPanel buildScriptPanel(JSONArray actions) {

        JPanel panel = new JPanel(new BorderLayout());

        JPanel toolBar = buildToolBar();

        Object[] components = buildEditorPanel(formatTitle(scriptTitle), SwingConstants.LEFT, toJsonTextWithFormat(scriptJson));
        scriptEditor = (LanguageTextField) components[1];

        JPanel scriptPanel = (JPanel) components[0];
        panel.add(scriptPanel, BorderLayout.CENTER);

        if (actions != null) {

            for (int i = 0; i < actions.size(); i++) {

                JSONObject action = actions.getJSONObject(i);
                String title = action.getString("title");
                JSONObject script = action.getJSONObject("script");

                JButton button = new JButton(title);
                button.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        scriptEditor.setText(toJsonTextWithFormat(script));
                        JSONObject input = getInputJson();

                        String result = ExecuteJsonNodeUtil.executeCode(input, script, contextParam);

                        JSONObject output = JSONObject.parse(result);
                        setOutputJson(output);
                    }
                });
                toolBar.add(button);
            }
        }

        panel.add(toolBar, BorderLayout.NORTH);

        return panel;
    }

    @NotNull
    private JPanel buildToolBar() {
        JPanel toolBar = new JPanel();

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

    private JSONObject getScriptJson() {
        if (useGraphScript) {
            return scriptJson;
        }
        return JSONObject.parse(scriptEditor.getText());
    }

    private void setOutputJson(JSONObject output) {
        if (useForm) {
            outputForm.setFormData(output);
            outputForm.setFormDataToChrome(output);
        } else {
            outputEditor.setText(toJsonTextWithFormat(output));
        }
    }

    @NotNull
    private JSONObject getInputJson() {
        if (useForm) {
            return inputForm.getFormData();
        }
        return JSONObject.parse(inputEditor.getText());
    }

    private JComponent buildInputAndOutputForm(JSONObject input, JSONObject output) {

        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setDividerSize(3);
        splitPane.setBorder(null);

        inputForm = new JsonFormPanel(parseJsonSchema(input), input);

        JPanel inputPanel = new JPanel(new BorderLayout());
        inputPanel.add(new JLabel(formatTitle(inputTitle)), BorderLayout.NORTH);
        inputPanel.add(inputForm, BorderLayout.CENTER);
        splitPane.add(inputPanel);

        outputForm = new JsonFormPanel(parseJsonSchema(output), output);

        JPanel outputPanel = new JPanel(new BorderLayout());
        outputPanel.add(new JLabel(formatTitle(outputTitle), SwingConstants.RIGHT), BorderLayout.NORTH);
        outputPanel.add(outputForm, BorderLayout.CENTER);
        splitPane.add(outputPanel);

        adjustSplitPanel(splitPane);

        return splitPane;

    }

    /**
     * 格式化title 添加缩进
     *
     * @param title title
     * @return title
     */
    String formatTitle(String title) {
        return "  " + title + "  ";
    }

    private JComponent buildInputAndOutputEditor(JSONObject input, JSONObject output) {

        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setDividerSize(3);
        splitPane.setBorder(null);

        String text = toJsonTextWithFormat(input);

        Object[] inputComponents = buildEditorPanel(formatTitle(inputTitle), SwingConstants.LEFT, text);

        JPanel inputPanel = (JPanel) inputComponents[0];
        inputEditor = (LanguageTextField) inputComponents[1];

        splitPane.add(inputPanel);

        text = toJsonTextWithFormat(output);

        Object[] outputComponents = buildEditorPanel(formatTitle(outputTitle), SwingConstants.RIGHT, text);

        JPanel outputPanel = (JPanel) outputComponents[0];
        outputEditor = (LanguageTextField) outputComponents[1];

        splitPane.add(outputPanel);

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

    private Object[] buildEditorPanel(String title, int horizontalAlignment, String text) {

        JPanel editorPanel = new JPanel(new BorderLayout());

        LanguageTextField jsonEditor = new LanguageTextField(Json5Language.INSTANCE, project, text);
        jsonEditor.setFont(EditorUtil.getEditorFont());
        jsonEditor.setOneLineMode(false);

        JBScrollPane jbScrollPane = new JBScrollPane(jsonEditor);

        // 第一个加入，方便获取
        editorPanel.add(jbScrollPane, BorderLayout.CENTER);

        JLabel label = new JLabel(title, horizontalAlignment);
        label.setFont(EditorUtil.getEditorFont());
        editorPanel.add(label, BorderLayout.NORTH);

        return new Object[]{editorPanel, jsonEditor};
    }

}
