package my.lang.page.app;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.ui.components.JBScrollPane;
import com.intellij.ui.components.JBTextArea;

import javax.swing.*;
import java.awt.*;

/**
 * json原生form
 */
public class JsonNativeFormPanel extends JPanel {

    JSONObject formData;
    Dimension LABEL_DIMENSION = new Dimension(100, 35);

    public JsonNativeFormPanel(JSONObject formData) {

        GridLayout layout = new GridLayout(formData.keySet().size(), 1);
        setLayout(layout);
        layout.setVgap(3);
        for (String key : formData.keySet()) {
            JPanel itemPanel = new JPanel(new BorderLayout());
            JLabel label = new JLabel(key.concat("  :  "));
            label.setMinimumSize(LABEL_DIMENSION);
            label.setPreferredSize(LABEL_DIMENSION);
            label.setMaximumSize(LABEL_DIMENSION);
            label.setHorizontalAlignment(JLabel.RIGHT);
            label.setMinimumSize(new Dimension(100, 30));
            JBTextArea field = new JBTextArea(formData.getString(key));
            field.setAutoscrolls(true);
            JBScrollPane jbScrollPane = new JBScrollPane(field);
            itemPanel.add(label, BorderLayout.WEST);
            itemPanel.add(jbScrollPane, BorderLayout.CENTER);
            this.add(itemPanel);
        }
    }
}
