// This is a generated file. Not intended for manual editing.
package fit.intellij.json.psi.impl;

import fit.intellij.json.psi.JsonValue;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.navigation.ItemPresentation;

public class JsonPropertyImpl extends fit.intellij.json.psi.impl.JsonPropertyMixin implements fit.intellij.json.psi.JsonProperty {

  public JsonPropertyImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull fit.intellij.json.psi.JsonElementVisitor visitor) {
    visitor.visitProperty(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.json.psi.JsonElementVisitor) accept((fit.intellij.json.psi.JsonElementVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  public @NotNull String getName() {
    return fit.intellij.json.psi.impl.JsonPsiImplUtils.getName(this);
  }

  @Override
  public @NotNull fit.intellij.json.psi.JsonValue getNameElement() {
    return fit.intellij.json.psi.impl.JsonPsiImplUtils.getNameElement(this);
  }

  @Override
  public @Nullable JsonValue getValue() {
    return fit.intellij.json.psi.impl.JsonPsiImplUtils.getValue(this);
  }

  @Override
  public @Nullable ItemPresentation getPresentation() {
    return JsonPsiImplUtils.getPresentation(this);
  }

}
