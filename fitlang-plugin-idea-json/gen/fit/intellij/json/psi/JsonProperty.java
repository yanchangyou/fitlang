// This is a generated file. Not intended for manual editing.
package fit.intellij.json.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import fit.intellij.psi.PsiNamedElement;
import com.intellij.navigation.ItemPresentation;

public interface JsonProperty extends JsonElement, PsiNamedElement {

  @NotNull String getName();

  JsonValue getNameElement();

  JsonValue getValue();

  @Nullable ItemPresentation getPresentation();

}
