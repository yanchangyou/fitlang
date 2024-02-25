package my.lang.dialog;

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

    int width = 400;
    int height = 400;

    public GlobalConfigPanelDialog(JSONObject config, JSONObject option) {

        super(true);

        if (config != null) {
            this.config = config;
        }

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

        resetConfig();

        int width = option.getIntValue("width", this.width);
        int height = option.getIntValue("height", this.height);

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

    Dimension LABEL_DIMENSION = new Dimension(80, 35);

    public void resetConfig() {

        panel.removeAll();

        fieldMap.clear();

        for (String key : config.keySet()) {
            JPanel itemPanel = new JPanel();
            JLabel label = new JLabel(key.concat(":"));
            label.setMinimumSize(LABEL_DIMENSION);
            label.setPreferredSize(LABEL_DIMENSION);
            label.setMaximumSize(LABEL_DIMENSION);
            label.setHorizontalAlignment(JLabel.RIGHT);
            JTextField field = new JTextField(config.getString(key));
            field.setColumns(20);
            fieldMap.put(key, field);
            itemPanel.add(new JLabel("  "));
            itemPanel.add(label);
            itemPanel.add(field);
            panel.add(itemPanel);
        }

        int columns = 1;
        if (config.size() > 5) {
            width = 800;
            columns = 2;
        }

        panel.setLayout(new GridLayout((config.size() + columns - 1) / columns, columns));

        height = (config.size() / columns) * 50 + 20;

    }

    public JSONObject readConfig() {
        for (Map.Entry<String, JTextField> field : fieldMap.entrySet()) {
            config.put(field.getKey(), field.getValue().getText());
        }
        return config;
    }

}
