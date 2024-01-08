package my.lang.action;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import com.intellij.ui.content.Content;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;

public class NodeConfigPanel extends AnAction {

    public static JSONObject INIT_CONFIG = JSONObject.parse("{'name':'value'}");

    static NodeConfigPanel nodeConfigPanel = new NodeConfigPanel(INIT_CONFIG);

    private JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));

    Map<String, JTextField> fieldMap = new HashMap<>();

    JSONObject config;

    public JPanel getPanel() {
        return panel;
    }

    public NodeConfigPanel() {
    }

    public NodeConfigPanel(JSONObject config) {
        resetConfig(config, null);
    }

    public void resetConfig(JSONObject config, Project project) {

        if (config == null) {
            config = INIT_CONFIG;
        }

        if (this.config != null) {
            boolean isContainAllKey = true;

            for (String key : config.keySet()) {
                if (!this.config.containsKey(key)) {
                    isContainAllKey = false;
                }
            }
            if (isContainAllKey) {
                return;
            }
        }

        this.config = config;

        panel.removeAll();
        fieldMap.clear();

        for (String key : config.keySet()) {
            JPanel itemPanel = new JPanel();
            JLabel label = new JLabel(key.concat(":"));
            label.setSize(100, 40);
            JTextField field = new JTextField(config.getString(key));
            field.setColumns(20);
            fieldMap.put(key, field);
            itemPanel.add(new JLabel("   "));
            itemPanel.add(label);
            itemPanel.add(field);
            itemPanel.setSize(350, 42);
            panel.add(itemPanel);
        }

        panel.setSize(900, ((config.size() + 2) / 3) * 50);

        if (project != null) {
            ToolWindow toolWindow = ToolWindowManager.getInstance(project).getToolWindow("FitLang Console");
            Content content = toolWindow.getContentManager().getContent(panel);
            if (content == null) {
                content = toolWindow.getContentManager().getFactory().createContent(panel, "Config", true);
                toolWindow.getContentManager().addContent(content);
                content.fireAlert();
            }
        }
    }

    public JSONObject readConfig() {
        for (Map.Entry<String, JTextField> field : fieldMap.entrySet()) {
            config.put(field.getKey(), field.getValue().getText());
        }
        return config;
    }

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
    }
}
