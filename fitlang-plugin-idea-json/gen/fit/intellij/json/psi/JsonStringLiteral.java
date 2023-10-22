// This is a generated file. Not intended for manual editing.
package fit.intellij.json.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;

public interface JsonStringLiteral extends JsonLiteral {

  @Nullable
  JsonMyKeyword getMyKeyword();

  @NotNull List<Pair<TextRange, String>> getTextFragments();

  @NotNull String getValue();

  boolean isPropertyName();

}
