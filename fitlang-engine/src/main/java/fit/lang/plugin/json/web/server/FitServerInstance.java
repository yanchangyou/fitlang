package fit.lang.plugin.json.web.server;

import cn.hutool.http.server.SimpleServer;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;

public class FitServerInstance {

    String url;

    SimpleServer simpleServer;

    JSONArray serviceList = new JSONArray();

    private String serverDir;

    private String serverFile;

    private boolean isRunning;


    public SimpleServer getSimpleServer() {
        return simpleServer;
    }

    public void setSimpleServer(SimpleServer simpleServer) {
        this.simpleServer = simpleServer;
    }

    public int getPort() {
        return simpleServer.getAddress().getPort();
    }

    public JSONArray getServiceList() {
        return serviceList;
    }

    public void setServiceList(JSONArray serviceList) {
        this.serviceList = serviceList;
    }

    public String getServerDir() {
        return serverDir;
    }

    public void setServerDir(String serverDir) {
        this.serverDir = serverDir;
    }

    public String getServerFile() {
        return serverFile;
    }

    public void setServerFile(String serverFile) {
        this.serverFile = serverFile;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public boolean isRunning() {
        return isRunning;
    }

    public void setRunning(boolean running) {
        isRunning = running;
    }

    public JSONObject getDisplayInfo() {
        JSONObject info = new JSONObject();
        info.put("serverDir", serverDir);
        info.put("serverFile", serverFile);
        info.put("url", url);
        info.put("port", getPort());
        return info;
    }

}
