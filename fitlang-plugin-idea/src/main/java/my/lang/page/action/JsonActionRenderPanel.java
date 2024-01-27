package my.lang.page.action;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;

import javax.swing.*;
import java.awt.event.ActionEvent;

import static my.lang.action.RunCodeAction.implementIdeOperator;

public class JsonActionRenderPanel extends JPanel {

    JSONObject actionDefine;

    JSONObject contextParam;


    public JsonActionRenderPanel(JSONObject actionDefine, JSONObject contextParam) {

        super(true);

        this.actionDefine = actionDefine;

        JSONArray actions = actionDefine.getJSONArray("actions");

        if (actions == null) {
            return;
        }

        for (int i = 0; i < actions.size(); i++) {
            JSONObject action = actions.getJSONObject(i);
            String title = action.getString("title");
            JSONObject script = action.getJSONObject("script");

            JButton button = new JButton(title);
            button.addActionListener(new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {

                    implementIdeOperator(null);

                    String result = ExecuteJsonNodeUtil.executeCode(new JSONObject(), script, contextParam);

                    System.out.println(result);
                }
            });

            add(button);

        }

    }

}
