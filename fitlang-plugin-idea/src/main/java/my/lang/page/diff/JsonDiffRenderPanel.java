package my.lang.page.diff;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

import static my.lang.action.RunCodeAction.implementIdeOperator;

public class JsonDiffRenderPanel extends JPanel {

    Project project;

    JSONObject diffDefine;

    JSONObject contextParam;

    VirtualFile appFile;

    JsonInputPanel json1InputPanel;

    JsonInputPanel json2InputPanel;

    JsonDiffResultPanel jsonDiffResultPanel;

    String appTitle = "App";
    String input1Title = "Input1";
    String input2Title = "Input2";
    String diffResultTitle = "Result";

    String type = "json";

    public JsonDiffRenderPanel(@NotNull Project project, JSONObject diffDefine, VirtualFile appFile, JSONObject contextParam) {

        this.project = project;
        this.diffDefine = diffDefine;
        this.appFile = appFile;
        this.contextParam = contextParam;

        type = diffDefine.containsKey("type") ? diffDefine.getString("type") : type;

        appTitle = diffDefine.containsKey("title") ? diffDefine.getString("title") : appTitle;
        input1Title = diffDefine.containsKey("input1Title") ? diffDefine.getString("input1Title") : input1Title;
        input2Title = diffDefine.containsKey("input2Title") ? diffDefine.getString("input2Title") : input2Title;
        diffResultTitle = diffDefine.containsKey("scriptTitle") ? diffDefine.getString("scriptTitle") : diffResultTitle;

        JSONObject input1 = diffDefine.getJSONObject("input1");
        JSONObject input2 = diffDefine.getJSONObject("input2");

        if (input1 == null) {
            input1 = new JSONObject();
        }

        if (input2 == null) {
            input2 = new JSONObject();
        }

        setBorder(null);
        setLayout(new BorderLayout());

        setAppTitle(appTitle);

        JSplitPane splitPane = buildMainPanel(input1, input2);
        splitPane.setBorder(null);

        adjustSplitPanel(splitPane);

        implementIdeOperator(null);

    }

    @NotNull
    private JSplitPane buildMainPanel(JSONObject input, JSONObject output) {
        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        add(splitPane, BorderLayout.CENTER);

        JComponent inputOutputEditor = buildInputAndOutputObjectPanel(input, output);

        splitPane.add(inputOutputEditor);

        JComponent resultPanel = buildDiffResultPanel();

        splitPane.add(resultPanel);

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

    JPanel buildDiffResultPanel() {

        JSONObject json1 = JSONObject.parse(json1InputPanel.getJsonTextEditor().getText());

        JSONObject json2 = JSONObject.parse(json2InputPanel.getJsonTextEditor().getText());

        jsonDiffResultPanel = new JsonDiffResultPanel(project);

        jsonDiffResultPanel.showDiff(json1, json2);

        JPanel panel = new JPanel(new BorderLayout());

        JPanel toolBar = buildToolBar();

        panel.add(toolBar, BorderLayout.NORTH);
        panel.add(jsonDiffResultPanel, BorderLayout.CENTER);

        return panel;
    }

    @NotNull
    private JPanel buildToolBar() {
        JPanel toolBar = new JPanel();

        //add default Run Button
        {
            JButton button = new JButton("Run");
            button.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {

                    JSONObject json1 = JSONObject.parse(json1InputPanel.jsonTextEditor.getText());
                    JSONObject json2 = JSONObject.parse(json2InputPanel.jsonTextEditor.getText());

                    jsonDiffResultPanel.showDiff(json1, json2);

                }
            });
            toolBar.add(button);
        }
        return toolBar;
    }

    private JComponent buildInputAndOutputObjectPanel(JSONObject input, JSONObject output) {

        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setDividerSize(3);
        splitPane.setBorder(null);

        json1InputPanel = new JsonInputPanel(input, input1Title, SwingConstants.LEFT, project);

        json2InputPanel = new JsonInputPanel(output, input2Title, SwingConstants.RIGHT, project);

        splitPane.add(json1InputPanel);
        splitPane.add(json2InputPanel);

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
    }
}
