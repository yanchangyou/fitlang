package my.lang.page.web;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.ui.jcef.JBCefBrowser;

public interface DealFetchResult {

    boolean isSuccess(JBCefBrowser browser);

    boolean checkData(JSONObject data, JBCefBrowser browser);

    void doNext(JSONObject data, JBCefBrowser browser);

}
