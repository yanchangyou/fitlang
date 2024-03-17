package fit.lang.plugin.json.ide.jcef;

import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.ui.LanguageTextField;
import com.intellij.ui.jcef.JBCefBrowser;

import static fit.lang.plugin.json.ExecuteJsonNodeUtil.toJsonTextWithFormat;

/**
 *
 */
public class FitJcefManager {

    static LanguageTextField resultTextEditor;


    static JBCefBrowser browser;

    public static LanguageTextField getResultTextEditor() {
        return resultTextEditor;
    }

    public static void setResultTextEditor(LanguageTextField resultTextEditor) {
        FitJcefManager.resultTextEditor = resultTextEditor;
    }

    public static JBCefBrowser getBrowser() {
        return browser;
    }

    public static void setBrowser(JBCefBrowser browser) {
        FitJcefManager.browser = browser;
    }

    public static void open(String url) {
        browser.loadURL(url);
    }

    public static void callback(JSONObject result) {

        JSONObject resultJson = JSONObject.parseObject(resultTextEditor.getText());

        JSONArray results = resultJson.getJSONArray("result");
        if (results == null) {
            results = new JSONArray();
            resultJson.put("result", results);
        }
        results.add(result);
        ApplicationManager.getApplication().invokeLater(() -> {
            resultTextEditor.setText(toJsonTextWithFormat(resultJson));
        });
    }
}
