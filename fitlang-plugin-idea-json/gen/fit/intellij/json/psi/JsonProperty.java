// This is a generated file. Not intended for manual editing.
package fit.intellij.json.psi;

import org.jetbrains.annotations.*;
import com.intellij.psi.PsiNamedElement;
import com.intellij.navigation.ItemPresentation;

public interface JsonProperty extends JsonElement, PsiNamedElement {

  @NotNull String getName();

  @NotNull fit.intellij.json.psi.JsonValue getNameElement();

  @Nullable JsonValue getValue();

  @Nullable ItemPresentation getPresentation();

}
