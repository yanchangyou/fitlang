package my.lang.action;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.editor.Editor;
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
        resetConfig(config);
    }

    public void resetConfig(JSONObject config) {

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
            JTextField field = new JTextField(config.getString(key));
            field.setColumns(20);
            fieldMap.put(key, field);
            itemPanel.add(new JLabel("   "));
            itemPanel.add(label);
            itemPanel.add(field);
            panel.add(itemPanel);
        }

        panel.setSize(900, ((config.size() + 2) / 3) * 50);
    }

    public JSONObject readConfig() {
        for (Map.Entry<String, JTextField> field : fieldMap.entrySet()) {
            config.put(field.getKey(), field.getValue().getText());
        }
        return config;
    }

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
        Editor editor = e.getData(CommonDataKeys.EDITOR);
        if (editor != null) {
            editor.setHeaderComponent(NodeConfigPanel.nodeConfigPanel.getPanel());
        }
    }
}
