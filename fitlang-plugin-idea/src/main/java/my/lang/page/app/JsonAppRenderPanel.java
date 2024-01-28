package my.lang.page.app;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.json.json5.Json5Language;
import com.intellij.openapi.editor.ex.util.EditorUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.LanguageTextField;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;
import static my.lang.action.RunCodeAction.implementIdeOperator;

public class JsonAppRenderPanel extends JPanel {

    Project project;

    JSONObject actionDefine;

    JSONObject contextParam;

    VirtualFile actionFile;

    LanguageTextField inputEditor;
    LanguageTextField outputEditor;
    LanguageTextField scriptEditor;


    public JsonAppRenderPanel(@NotNull Project project, JSONObject appDefine, VirtualFile actionFile, JSONObject contextParam) {

        this.project = project;

        this.actionDefine = appDefine;
        this.contextParam = contextParam;
        this.actionFile = actionFile;

        String title = appDefine.getString("title");

        JSONArray actions = appDefine.getJSONArray("actions");

        JSONObject input = appDefine.getJSONObject("input");
        JSONObject output = appDefine.getJSONObject("output");

        setBorder(null);
        setLayout(new BorderLayout());

        setAppTitle(title);

        JSplitPane splitPane = buildMainPanel(input, output, actions);
        splitPane.setBorder(null);

        adjustSplitPanel(splitPane);

        implementIdeOperator(null);

    }

    @NotNull
    private JSplitPane buildMainPanel(JSONObject input, JSONObject output, JSONArray actions) {
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        add(splitPane, BorderLayout.CENTER);

        JComponent inputOutputEditor = buildInputAndOutputEditor(input, output);

        splitPane.add(inputOutputEditor);

        JComponent scriptPanel = buildScriptPanel(actions);

        splitPane.add(scriptPanel);
        return splitPane;
    }

    private void setAppTitle(String title) {
        if (StrUtil.isBlank(title)) {
            title = "App";
        }
        JLabel titleLabel = new JLabel(title, SwingConstants.CENTER);
        titleLabel.setAlignmentX(15);
        titleLabel.setAlignmentY(15);
        JPanel panel = new JPanel(new BorderLayout());
        panel.add(new JLabel(" "), BorderLayout.NORTH);
        panel.add(titleLabel, BorderLayout.CENTER);
        add(panel, BorderLayout.NORTH);
    }

    JPanel buildScriptPanel(JSONArray actions) {

        JPanel toolBar = new JPanel();
        toolBar.setAlignmentX(15);

        JSONObject helloNode = new JSONObject();
        helloNode.put("uni", "hello");

        JPanel scriptPanel = buildEditorPanel("", toJsonTextWithFormat(helloNode));

        scriptEditor = getEditor(scriptPanel);

        JPanel panel = new JPanel(new BorderLayout());

        panel.add(scriptPanel, BorderLayout.CENTER);

        //add default Run Button
        {
            JButton button = new JButton("Run");
            button.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {

                    JSONObject script = JSONObject.parse(scriptEditor.getText());

                    JSONObject input = JSONObject.parse(inputEditor.getText());

                    String result = ExecuteJsonNodeUtil.executeCode(input, script, contextParam);

                    JSONObject output = JSONObject.parse(result);

                    outputEditor.setText(toJsonTextWithFormat(output));

                }
            });
            toolBar.add(button);
        }

        if (actions != null) {

            for (int i = 0; i < actions.size(); i++) {

                JSONObject action = actions.getJSONObject(i);
                String title = action.getString("title");
                JSONObject script = action.getJSONObject("script");

                JButton button = new JButton(title);
                button.addActionListener(new AbstractAction() {
                    @Override
                    public void actionPerformed(ActionEvent actionEvent) {

                        JsonAppRenderPanel.getEditor(scriptPanel).setText(toJsonTextWithFormat(script));

                        JSONObject input = JSONObject.parse(outputEditor.getText());

                        String result = ExecuteJsonNodeUtil.executeCode(input, script, contextParam);

                        JSONObject output = JSONObject.parse(result);

                        outputEditor.setText(toJsonTextWithFormat(output));

                    }
                });
                toolBar.add(button);
            }
        }

        panel.add(toolBar, BorderLayout.NORTH);

        return panel;
    }

    @NotNull
    private static LanguageTextField getEditor(JComponent component) {
        return (LanguageTextField) getFirstComponent(component);
    }

    @NotNull
    private static Component getFirstComponent(JComponent component) {
        return component.getComponent(0);
    }

    private JComponent buildInputAndOutputEditor(JSONObject input, JSONObject output) {

        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setDividerSize(3);
        splitPane.setBorder(null);

        String text = toJsonTextWithFormat(input);

        JPanel inputPanel = buildEditorPanel("Input JSON", text);

        inputEditor = getEditor(inputPanel);

        splitPane.add(inputPanel);

        text = toJsonTextWithFormat(output);

        JPanel outputPanel = buildEditorPanel("Output JSON", text);
        outputEditor = getEditor(outputPanel);

        splitPane.add(outputPanel);

        adjustSplitPanel(splitPane);

        return splitPane;

    }

    private static void adjustSplitPanel(JSplitPane splitPane) {
        new Thread() {
            @Override
            public void run() {
                for (int i = 0; i < 10; i++) {
                    try {
                        Thread.sleep(500L);
                    } catch (InterruptedException e) {
                        throw new RuntimeException(e);
                    }
                    splitPane.setDividerLocation(0.5);
                }
            }
        }.start();
    }

    @NotNull
    private JPanel buildEditorPanel(String title, String text) {

        JPanel editorPanel = new JPanel(new BorderLayout());

        LanguageTextField inputEditor = new LanguageTextField(Json5Language.INSTANCE, project, text);
        inputEditor.setFont(EditorUtil.getEditorFont());
        inputEditor.setOneLineMode(false);

        // 第一个加入，方便获取
        editorPanel.add(inputEditor, BorderLayout.CENTER);

        JLabel label = new JLabel(title, SwingConstants.CENTER);
        label.setFont(EditorUtil.getEditorFont());
        editorPanel.add(label, BorderLayout.NORTH);

        return editorPanel;
    }

}
