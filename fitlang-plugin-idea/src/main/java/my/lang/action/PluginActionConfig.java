package my.lang.action;

import com.alibaba.fastjson2.JSONObject;

/**
 * @author yanchangyou
 */
public class PluginActionConfig {

    String name;

    String title;

    String shortCut1;

    String shortCut2;

    JSONObject script;

    boolean refresh;

    boolean refreshParent;

    boolean visible;

    protected PluginActionConfig(JSONObject config) {
        this.name = config.getString("name");
        this.title = (String) config.getOrDefault("title", name);
        this.shortCut1 = config.getString("shortCut1");
        this.shortCut2 = config.getString("shortCut2");

        this.script = config.getJSONObject("script");
        this.refresh = Boolean.TRUE.equals(config.getBoolean("refresh"));
        this.refreshParent = Boolean.TRUE.equals(config.getBoolean("refreshParent"));
        this.visible = !Boolean.FALSE.equals(config.getBoolean("visible"));
    }

    public String getId() {
        return "Fit Action ".concat(title);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public JSONObject getScript() {
        return script;
    }

    public void setScript(JSONObject script) {
        this.script = script;
    }

    public boolean isRefresh() {
        return refresh;
    }

    public void setRefresh(boolean refresh) {
        this.refresh = refresh;
    }

    public boolean isRefreshParent() {
        return refreshParent;
    }

    public void setRefreshParent(boolean refreshParent) {
        this.refreshParent = refreshParent;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public boolean isVisible() {
        return visible;
    }

    public void setVisible(boolean visible) {
        this.visible = visible;
    }
}