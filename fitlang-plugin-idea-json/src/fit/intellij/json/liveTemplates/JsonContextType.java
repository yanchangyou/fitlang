package fit.intellij.json.liveTemplates;

import com.intellij.codeInsight.template.FileTypeBasedContextType;
import fit.intellij.json.JsonBundle;
import fit.intellij.json.JsonFileType;
import fit.intellij.json.psi.JsonFile;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

/**
 * @author Konstantin.Ulitin
 */
public class JsonContextType extends FileTypeBasedContextType {
  protected JsonContextType() {
    super("FitLang", JsonBundle.message("json.template.context.type"), JsonFileType.INSTANCE);
  }

  @Override
  public boolean isInContext(@NotNull PsiFile file, int offset) {
    return file instanceof JsonFile;
  }
}
