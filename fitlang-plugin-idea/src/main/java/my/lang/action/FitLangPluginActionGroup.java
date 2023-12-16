package my.lang.action;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.actionSystem.*;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.isJsonObjectText;
import static my.lang.action.FitLangPluginAction.actionPerformedInner;

public abstract class FitLangPluginActionGroup extends DefaultActionGroup {

    AnAction[] children = new AnAction[0];

    JSONObject actionScript;

    private final BlockingQueue<Runnable> workQueue = new ArrayBlockingQueue<>(100);

    ThreadPoolExecutor threadPoolExecutor = new ThreadPoolExecutor(8, 100, 100, TimeUnit.MINUTES, workQueue);

    public boolean canBePerformed(@NotNull DataContext context) {
        return actionScript != null;
    }

    @Override
    public void actionPerformed(@NotNull AnActionEvent e) {
        if (actionScript != null) {
            actionPerformedInner(e, threadPoolExecutor, actionScript);
        }
    }

    @Override
    public void update(AnActionEvent event) {
        if (children == null || children.length == 0 || debug) {

            event.getPresentation().setEnabledAndVisible(false);

            String projectPath = event.getProject() == null ? "" : event.getProject().getBasePath();
            if (StrUtil.isBlank(projectPath)) {
                return;
            }
            JSONObject pluginConfig = loadPluginRegisterConfig(projectPath);
            if (pluginConfig == null || pluginConfig.isEmpty()) {
                return;
            }
            JSONObject groupConfig = getGroupConfig(pluginConfig);
            if (groupConfig == null || groupConfig.isEmpty()) {
                return;
            }
            actionScript = groupConfig.getJSONObject("script");
            if (actionScript != null) {
                event.getPresentation().setEnabledAndVisible(true);
                String title = groupConfig.getString("title");
                if (StrUtil.isBlank(title)) {
                    title = groupConfig.getString("name");
                }
                event.getPresentation().setText(title);
                return;
            }
            JSONArray actions = groupConfig.getJSONArray("actions");
            if (actions == null || actions.isEmpty()) {
                return;
            }
            event.getPresentation().setText(groupConfig.getString("title"));
            event.getPresentation().setEnabledAndVisible(true);
            debug = Boolean.TRUE.equals(pluginConfig.getBoolean("debug"));
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
                JSONObject script = plugin.getJSONObject("script");
                actionList.add(new FitLangPluginAction(name, title, script));
            }

            System.out.println("FitLang: loaded plugin: ".concat(pluginNameSet.toString()));

            children = actionList.toArray(new AnAction[0]);
        }
    }

    boolean debug = false;

    protected abstract String getGroupName();

    JSONObject getGroupConfig(JSONObject pluginConfig) {
        JSONArray groups = pluginConfig.getJSONArray("groups");
        if (groups == null || groups.isEmpty()) {
            return null;
        }
        //倒序加载，每次都放到最前
        for (int i = groups.size() - 1; i > -1; i--) {
            JSONObject groupObject = groups.getJSONObject(i);
            if (getGroupName().equalsIgnoreCase(groupObject.getString("name"))) {
                if (!(groupObject.get("actions") instanceof JSONArray)
                        && !(groupObject.get("script") instanceof JSONObject)) {
                    return null;
                }
                JSONObject groupConfig = new JSONObject();
                groupConfig.put("title", groupObject.getOrDefault("title", getGroupName()));
                groupConfig.put("actions", groupObject.get("actions"));
                groupConfig.put("script", groupObject.get("script"));
                return groupConfig;
            }
        }
        return null;
    }

    @Override
    public AnAction[] getChildren(AnActionEvent event) {
        return children;
    }

    JSONObject loadPluginRegisterConfig(String projectPath) {
        try {

            String pluginPath = projectPath + File.separator + "plugin.fit";
            System.out.println("FitLang: load plugin at ".concat(pluginPath));

            String pluginConfigJson = FileUtil.readUtf8String(pluginPath);

            if (isJsonObjectText(pluginConfigJson)) {
                return JSONObject.parse(pluginConfigJson);
            }
        } catch (Exception e) {
            //ignore
        }
        return null;
    }
}