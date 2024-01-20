// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi.impl;

import java.util.List;

import fit.intellij.jsonpath.psi.JsonPathPsiUtils;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import fit.intellij.jsonpath.psi.JsonPathStringLiteralMixin;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;

public class JsonPathStringLiteralImpl extends JsonPathStringLiteralMixin implements fit.intellij.jsonpath.psi.JsonPathStringLiteral {

  public JsonPathStringLiteralImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull fit.intellij.jsonpath.psi.JsonPathVisitor visitor) {
    visitor.visitStringLiteral(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.jsonpath.psi.JsonPathVisitor) accept((fit.intellij.jsonpath.psi.JsonPathVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  public @NotNull List<Pair<TextRange, String>> getTextFragments() {
    return JsonPathPsiUtils.getTextFragments(this);
  }

}
