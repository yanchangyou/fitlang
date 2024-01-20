// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi.impl;

import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;

public class JsonPathRegexExpressionImpl extends JsonPathExpressionImpl implements fit.intellij.jsonpath.psi.JsonPathRegexExpression {

  public JsonPathRegexExpressionImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull fit.intellij.jsonpath.psi.JsonPathVisitor visitor) {
    visitor.visitRegexExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.jsonpath.psi.JsonPathVisitor) accept((fit.intellij.jsonpath.psi.JsonPathVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public fit.intellij.jsonpath.psi.JsonPathExpression getExpression() {
    return findNotNullChildByClass(fit.intellij.jsonpath.psi.JsonPathExpression.class);
  }

  @Override
  @NotNull
  public fit.intellij.jsonpath.psi.JsonPathRegexLiteral getRegexLiteral() {
    return findNotNullChildByClass(fit.intellij.jsonpath.psi.JsonPathRegexLiteral.class);
  }

}
