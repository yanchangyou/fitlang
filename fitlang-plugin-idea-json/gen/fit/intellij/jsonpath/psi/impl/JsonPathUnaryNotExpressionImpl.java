// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi.impl;

import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;

public class JsonPathUnaryNotExpressionImpl extends JsonPathExpressionImpl implements fit.intellij.jsonpath.psi.JsonPathUnaryNotExpression {

  public JsonPathUnaryNotExpressionImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull fit.intellij.jsonpath.psi.JsonPathVisitor visitor) {
    visitor.visitUnaryNotExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.jsonpath.psi.JsonPathVisitor) accept((fit.intellij.jsonpath.psi.JsonPathVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public fit.intellij.jsonpath.psi.JsonPathExpression getExpression() {
    return findChildByClass(fit.intellij.jsonpath.psi.JsonPathExpression.class);
  }

}
