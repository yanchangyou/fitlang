package my.lang.page.action;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.alibaba.fastjson2.JSONWriter;
import com.intellij.openapi.vfs.VirtualFile;
import fit.lang.plugin.json.ExecuteJsonNodeUtil;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;
import static fit.lang.plugin.json.ExecuteJsonNodeUtil.readNodeDefineFile;
import static my.lang.action.RunCodeAction.implementIdeOperator;

public class JsonActionRenderPanel extends JPanel {

    JSONObject actionDefine;

    JSONObject contextParam;

    VirtualFile actionFile;

    String inputFilePath = "input.json";
    String outputFilePath = "output.json";

    public JsonActionRenderPanel(JSONObject actionDefine, VirtualFile actionFile, JSONObject contextParam) {

        super(true);

        this.actionDefine = actionDefine;
        this.contextParam = contextParam;
        this.actionFile = actionFile;

        if (actionDefine.containsKey("inputFile")) {
            inputFilePath = actionDefine.getString("inputFile");
        }

        if (actionDefine.containsKey("outputFile")) {
            outputFilePath = actionDefine.getString("outputFile");
        }

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

                    implementIdeOperator(null, null);

                    JSONObject input = getActionInput();
                    String result = ExecuteJsonNodeUtil.executeCode(input, script, contextParam);
                    System.out.println(result);

                    JSONObject output = JSONObject.parse(result);
                    try {
                        setActionOutput(output);
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }

                }
            });

            add(button);

        }
    }

    JSONObject getActionInput() {
        JSONObject result = new JSONObject();
        String inputPath = actionFile.getParent().getPath().concat(File.separator).concat(inputFilePath);
        File inputFile = new File(inputPath);
        if (!inputFile.exists()) {
            return result;
        }

        String content = readNodeDefineFile(inputFile);
        if (!isJsonObjectText(content)) {
            result.put("data", content);
            return result;
        }

        return JSONObject.parse(content);
    }

    void setActionOutput(JSONObject output) throws IOException {
        String outputPath = actionFile.getParent().getPath().concat(File.separator).concat(outputFilePath);
        File outputFile = new File(outputPath);
        FileUtil.writeUtf8String(output.toJSONString(JSONWriter.Feature.PrettyFormat), outputFile);

        //实时刷新文件
        VirtualFile outputVFile = actionFile.getParent().findChild(outputFilePath);
        if (outputVFile != null) {
            outputVFile.refresh(false, false);
        }
    }

    public void dispose() {

    }
}
