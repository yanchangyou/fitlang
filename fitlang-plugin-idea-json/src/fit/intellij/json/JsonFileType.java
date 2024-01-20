package fit.intellij.json;

import com.intellij.lang.Language;
import com.intellij.openapi.fileTypes.LanguageFileType;
import my.lang.icon.MyLanguageIcons;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

/**
 * @author Mikhail Golubev
 */
public class JsonFileType extends LanguageFileType {
  public static final JsonFileType INSTANCE = new JsonFileType();
  public static final String DEFAULT_EXTENSION = "fit";

  protected JsonFileType(Language language) {
    super(language);
  }

  protected JsonFileType(Language language, boolean secondary) {
    super(language, secondary);
  }

  protected JsonFileType() {
    super(JsonLanguage.INSTANCE);
  }

  @NotNull
  @Override
  public String getName() {
    return "FitLang";
  }

  @NotNull
  @Override
  public String getDescription() {
    return "FitLang";
  }

  @NotNull
  @Override
  public String getDefaultExtension() {
    return DEFAULT_EXTENSION;
  }

  @Override
  public Icon getIcon() {
    return MyLanguageIcons.FILE;
  }
}
