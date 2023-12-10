package my.lang.action;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DefaultActionGroup;
import org.jetbrains.annotations.NotNull;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;
import static my.lang.action.RunCodeAction.getPluginFile;

public class FitLangToolActionGroup extends DefaultActionGroup {

    @Override
    public void update(AnActionEvent event) {
        event.getPresentation().setEnabled(true);
    }

    AnAction[] children;

    @NotNull
    @Override
    public AnAction[] getChildren(AnActionEvent event) {
        if (children == null) {
            JSONObject pluginConfig = loadPluginRegisterConfig();
            JSONArray plugins = pluginConfig.getJSONArray("plugins");
            children = new AnAction[plugins.size()];
            int index = 0;
            for (Object item : plugins) {
                JSONObject plugin = (JSONObject) item;
                String name = plugin.getString("name");
                String title = plugin.getString("title");
                children[index++] = new FitLangToolAction(name, title);
            }
        }
        return children;
    }

    JSONObject loadPluginRegisterConfig() {
        String fitPath = "fit/plugin/tools/plugin.json";
        String pluginConfigJson = getPluginFile(fitPath);

        if (isJsonObjectText(pluginConfigJson)) {
            return JSONObject.parse(pluginConfigJson);
        } else {
            throw new RuntimeException("load plugin.json error!");
        }
    }
}