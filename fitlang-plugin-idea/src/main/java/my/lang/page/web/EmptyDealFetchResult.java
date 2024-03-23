package my.lang.page.web;

import com.alibaba.fastjson2.JSONObject;
import com.intellij.ui.jcef.JBCefBrowser;

public class EmptyDealFetchResult implements DealFetchResult {
    @Override
    public boolean check(JSONObject data, JBCefBrowser browser) {
        return true;
    }

    @Override
    public void doNext(JSONObject data, JBCefBrowser browser) {

    }
}
