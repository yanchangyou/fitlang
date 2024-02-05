package my.lang.page.diff;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;
import static my.lang.action.RunCodeAction.implementIdeOperator;

public class JsonDiffRenderPanel extends JPanel {

    Project project;

    JSONObject diffDefine;

    JSONObject contextParam;

    VirtualFile appFile;

    JsonInputPanel json1InputPanel;

    JsonInputPanel json2InputPanel;

    JsonDiffResultPanel jsonDiffResultPanel;

    JCheckBox isNeedSort = new JCheckBox("字段排序");

    String appTitle = "Diff";
    String input1Title = "Input1";
    String input2Title = "Input2";
    String diffResultTitle = "Result";

    String defaultButtonTitle = "Compare";

    String type = "json";
    JButton button;

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
        defaultButtonTitle = diffDefine.containsKey("defaultButtonTitle") ? diffDefine.getString("defaultButtonTitle") : defaultButtonTitle;

        Object input1 = diffDefine.get("input1");
        Object input2 = diffDefine.get("input2");

        setBorder(null);
        setLayout(new BorderLayout());

        setAppTitle(appTitle);

        JSplitPane splitPane = buildMainPanel(input1, input2);
        splitPane.setBorder(null);

        adjustSplitPanel(splitPane);

        implementIdeOperator(null);

    }

    @NotNull
    private JSplitPane buildMainPanel(Object input, Object output) {
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

        jsonDiffResultPanel = new JsonDiffResultPanel(project);

        JPanel panel = new JPanel(new BorderLayout());

        JPanel toolBar = buildToolBar();

        panel.add(toolBar, BorderLayout.NORTH);
        panel.add(jsonDiffResultPanel, BorderLayout.CENTER);

        button.doClick();

        return panel;
    }

    @NotNull
    private JPanel buildToolBar() {
        JPanel toolBar = new JPanel();

        toolBar.add(isNeedSort);
        //add default Run Button
        {
            button = new JButton(defaultButtonTitle);
            button.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {

                    Object input1 = json1InputPanel.jsonTextEditor.getText();
                    Object input2 = json2InputPanel.jsonTextEditor.getText();

                    if (isJsonObjectText(input1)) {
                        input1 = JSONObject.parse(input1.toString());
                    }

                    if (isJsonObjectText(input2)) {
                        input2 = JSONObject.parse(input2.toString());
                    }

                    jsonDiffResultPanel.showDiff(input1, input2, isNeedSort.isSelected());

                }
            });
            toolBar.add(button);
        }
        return toolBar;
    }

    private JComponent buildInputAndOutputObjectPanel(Object input1, Object input2) {

        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        splitPane.setDividerSize(3);
        splitPane.setBorder(null);

        json1InputPanel = new JsonInputPanel(input1, input1Title, SwingConstants.LEFT, project);

        json2InputPanel = new JsonInputPanel(input2, input2Title, SwingConstants.RIGHT, project);

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
