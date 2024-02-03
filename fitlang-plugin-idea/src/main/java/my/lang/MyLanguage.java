package my.lang;

import com.intellij.lang.Language;
import fit.lang.FitLang;
import fit.lang.plugin.json.ide.FitPluginInit;

/**
 * @author yanchangyou
 */
public class MyLanguage extends Language {

    public static final String LANG_STRING_LOGO = "" +
            "FitLang version: " + FitLang.VERSION +
            "\n";
    public static final String LANG_NAME = "FitLang";

    public static final String LANG_FILE_SUFFIX = "fit";

    public static final String LANG_FILE_SUFFIX_WITH_DOT = ".".concat(LANG_FILE_SUFFIX);

    public static final MyLanguage INSTANCE = new MyLanguage();

    private MyLanguage() {
        super(LANG_FILE_SUFFIX);
    }

    public static boolean isFitLanguageFile(String fileName) {
        return fileName != null && (fileName.endsWith(LANG_FILE_SUFFIX_WITH_DOT) || fileName.endsWith(LANG_FILE_SUFFIX_WITH_DOT.toUpperCase()) || fileName.endsWith(LANG_FILE_SUFFIX_WITH_DOT.concat(".json")));
    }

    //初始化，后续优化
    static {
        FitPluginInit.init();
    }
}
