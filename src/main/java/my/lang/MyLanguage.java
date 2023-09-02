package my.lang;

import com.intellij.lang.Language;

/**
 * @author yanchangyou
 */
public class MyLanguage extends Language {

    public static final String LANG_STRING_LOGO = "" +
            "  _____    _    _   \n" +
            " |  ___|  (_)  | |_ \n" +
            " | |_     | |  | __|\n" +
            " |  _|    | |  | |_ \n" +
            " |_|      |_|   \\__|\n";
    public static final String LANG_NAME = "FitLang";

    public static final String LANG_FILE_SUFFIX = "fit";

    public static final String LANG_FILE_SUFFIX_WITH_DOT = ".".concat(LANG_FILE_SUFFIX);

    public static final MyLanguage INSTANCE = new MyLanguage();

    private MyLanguage() {
        super(LANG_FILE_SUFFIX);
    }

    public static boolean isMyLanguageFile(String fileName) {
        return fileName != null && (fileName.endsWith(LANG_FILE_SUFFIX_WITH_DOT) || fileName.endsWith(LANG_FILE_SUFFIX_WITH_DOT.concat(".json")));
    }
}
