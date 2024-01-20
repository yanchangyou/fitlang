// This is a generated file. Not intended for manual editing.
package fit.intellij.json.psi.impl;

import java.util.List;

import fit.intellij.json.psi.JsonElementVisitor;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;

public class JsonStringLiteralImpl extends JsonStringLiteralMixin implements fit.intellij.json.psi.JsonStringLiteral {

  public JsonStringLiteralImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull fit.intellij.json.psi.JsonElementVisitor visitor) {
    visitor.visitStringLiteral(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.json.psi.JsonElementVisitor) accept((JsonElementVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  public @NotNull List<Pair<TextRange, String>> getTextFragments() {
    return fit.intellij.json.psi.impl.JsonPsiImplUtils.getTextFragments(this);
  }

  @Override
  public @NotNull String getValue() {
    return fit.intellij.json.psi.impl.JsonPsiImplUtils.getValue(this);
  }

  @Override
  public boolean isPropertyName() {
    return JsonPsiImplUtils.isPropertyName(this);
  }

}
