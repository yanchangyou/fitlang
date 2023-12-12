package my.lang.action;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DefaultActionGroup;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;
import static my.lang.action.RunCodeAction.getPluginFile;

public class FitLangPluginActionGroup extends DefaultActionGroup {

    @Override
    public void update(AnActionEvent event) {
        event.getPresentation().setEnabled(true);
    }

    AnAction[] children = new AnAction[0];

    boolean isDebug = false;

    @Override
    public AnAction[] getChildren(AnActionEvent event) {
        if (children == null || children.length == 0 || isDebug) {
            String projectPath = event.getProject() == null ? "" : event.getProject().getBasePath();
            JSONObject pluginConfig = loadPluginRegisterConfig(projectPath);
            if (pluginConfig == null || pluginConfig.isEmpty()) {
                return children;
            }
            JSONArray actions = pluginConfig.getJSONArray("action");
            if (actions == null) {
                return children;
            }
            isDebug = Boolean.TRUE.equals(pluginConfig.getBoolean("debug"));
            List<AnAction> actionList = new ArrayList<>();
            Set<String> pluginNameSet = new HashSet<>();
            for (Object item : actions) {
                JSONObject plugin = (JSONObject) item;
                String name = plugin.getString("name");
                if (pluginNameSet.contains(name)) {
                    continue;
                }
                pluginNameSet.add(name);
                String title = plugin.getString("title");
                String script = plugin.getString("script");
                actionList.add(new FitLangPluginAction(name, title, script));
            }
            children = actionList.toArray(new AnAction[0]);
        }
        return children;
    }

    JSONObject loadPluginRegisterConfig(String projectPath) {

        try {

            String pluginConfigJson = FileUtil.readUtf8String(projectPath + File.separator + "plugin.fit");

            if (isJsonObjectText(pluginConfigJson)) {
                return JSONObject.parse(pluginConfigJson);
            }
        } catch (Exception e) {
            //ignore
        }
        return null;
    }
}