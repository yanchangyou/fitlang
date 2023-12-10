package my.lang.action;

import cn.hutool.core.io.FileUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DefaultActionGroup;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;
import static my.lang.action.RunCodeAction.getPluginFile;

public class FitLangToolActionGroup extends DefaultActionGroup {

    @Override
    public void update(AnActionEvent event) {
        event.getPresentation().setEnabled(true);
    }

    AnAction[] children;

    boolean isDebug = false;

    @Override
    public AnAction[] getChildren(AnActionEvent event) {
        if (children == null || isDebug) {
            JSONObject pluginConfig = loadPluginRegisterConfig();
            JSONArray plugins = pluginConfig.getJSONArray("plugins");
            List<AnAction> actionList = new ArrayList<>();
            Set<String> pluginNameSet = new HashSet<>();
            for (Object item : plugins) {
                JSONObject plugin = (JSONObject) item;
                String name = plugin.getString("name");
                if (pluginNameSet.contains(name)) {
                    continue;
                }
                pluginNameSet.add(name);
                String title = plugin.getString("title");
                String fitPath = plugin.getString("fitPath");
                actionList.add(new FitLangToolAction(name, title, fitPath));
            }
            children = actionList.toArray(new AnAction[0]);
        }
        return children;
    }

    JSONObject loadPluginRegisterConfig() {
        String[] pluginPaths = new String[]{
                "~/plugin.fit.json",
                "fit/plugin/tools/plugin.json",
        };
        JSONArray plugins = new JSONArray();
        for (String pluginPath : pluginPaths) {
            JSONObject pluginConfigJson = null;
            try {
                pluginConfigJson = loadPluginRegisterConfig(pluginPath);
            } catch (Exception e) {
                System.out.println(e.getMessage());
            }
            if (pluginConfigJson == null) {
                try {
                    String config = FileUtil.readUtf8String(pluginPath);
                    if (!isJsonObjectText(config)) {
                        continue;
                    }
                    pluginConfigJson = JSONObject.parse(config);
                } catch (Exception e) {
                    System.out.println(e.getMessage());
                }
            }

            if (pluginConfigJson != null) {
                if (pluginConfigJson.getJSONArray("plugins") != null) {
                    plugins.addAll(pluginConfigJson.getJSONArray("plugins"));
                }
                if (!isDebug) {
                    isDebug =Boolean.TRUE.equals(pluginConfigJson.getBoolean("isDebug"));
                }
            }
        }
        JSONObject pluginConfig = new JSONObject();
        pluginConfig.put("plugins", plugins);
        pluginConfig.put("isDebug", isDebug);
        return pluginConfig;
    }

    JSONObject loadPluginRegisterConfig(String pluginPath) {

        String pluginConfigJson = getPluginFile(pluginPath);

        if (isJsonObjectText(pluginConfigJson)) {
            return JSONObject.parse(pluginConfigJson);
        }
        return null;
    }
}