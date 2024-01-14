package my.lang.action;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.ui.DialogWrapper;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;

public class GlobalConfigPanelDialog extends DialogWrapper {

    public static JSONObject INIT_CONFIG = JSONObject.parse("{'name':'value'}");

    private final JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT));

    Map<String, JTextField> fieldMap = new HashMap<>();

    JSONObject config = INIT_CONFIG;
    JSONObject option;

    int height = 400;

    public GlobalConfigPanelDialog(JSONObject config, JSONObject option) {

        super(true);

        if (config != null) {
            this.config = config;
        }
        resetConfig();

        if (option == null) {
            option = new JSONObject();
        }

        String title = "Config Form";
        if (option.containsKey("title")) {
            title = option.getString("title");
        }
        setTitle(title);

        this.setModal(!Boolean.FALSE.equals(option.getBoolean("modal")));

        this.option = option;

        int width = option.getIntValue("width", 800);

        setSize(width, height);

        init();
    }

    public JPanel getPanel() {
        return panel;
    }

    @Nullable
    @Override
    protected JComponent createCenterPanel() {
        return getPanel();
    }

    public void resetConfig() {

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

        height = ((config.size() + 4) / 2) * 50;

    }

    public JSONObject readConfig() {
        for (Map.Entry<String, JTextField> field : fieldMap.entrySet()) {
            config.put(field.getKey(), field.getValue().getText());
        }
        return config;
    }

}
