package fit.intellij.json;

import com.intellij.lang.Language;

public class JsonLanguage extends Language {
  public static final JsonLanguage INSTANCE = new JsonLanguage();

  protected JsonLanguage(String ID, String... mimeTypes) {
    super(INSTANCE, ID, mimeTypes);
  }

    private JsonLanguage() {
        super("FitLang");
    }

  @Override
  public boolean isCaseSensitive() {
    return true;
  }

  public boolean hasPermissiveStrings() { return false; }
}
