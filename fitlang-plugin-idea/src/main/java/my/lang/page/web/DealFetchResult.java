package my.lang.page.web;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.ui.jcef.JBCefBrowser;

public interface DealFetchResult {

    boolean check(JSONObject data, JBCefBrowser browser);

    void doNext(JSONObject data, JBCefBrowser browser);

}
