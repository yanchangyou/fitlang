// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi.impl;

import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.extapi.psi.ASTWrapperPsiElement;

public class JsonPathExpressionSegmentImpl extends ASTWrapperPsiElement implements fit.intellij.jsonpath.psi.JsonPathExpressionSegment {

  public JsonPathExpressionSegmentImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull fit.intellij.jsonpath.psi.JsonPathVisitor visitor) {
    visitor.visitExpressionSegment(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.jsonpath.psi.JsonPathVisitor) accept((fit.intellij.jsonpath.psi.JsonPathVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public fit.intellij.jsonpath.psi.JsonPathFilterExpression getFilterExpression() {
    return findChildByClass(fit.intellij.jsonpath.psi.JsonPathFilterExpression.class);
  }

  @Override
  @Nullable
  public fit.intellij.jsonpath.psi.JsonPathIndexExpression getIndexExpression() {
    return findChildByClass(fit.intellij.jsonpath.psi.JsonPathIndexExpression.class);
  }

  @Override
  @Nullable
  public fit.intellij.jsonpath.psi.JsonPathIndexesList getIndexesList() {
    return findChildByClass(fit.intellij.jsonpath.psi.JsonPathIndexesList.class);
  }

  @Override
  @Nullable
  public fit.intellij.jsonpath.psi.JsonPathQuotedPathsList getQuotedPathsList() {
    return findChildByClass(fit.intellij.jsonpath.psi.JsonPathQuotedPathsList.class);
  }

  @Override
  @Nullable
  public fit.intellij.jsonpath.psi.JsonPathSliceExpression getSliceExpression() {
    return findChildByClass(fit.intellij.jsonpath.psi.JsonPathSliceExpression.class);
  }

  @Override
  @Nullable
  public fit.intellij.jsonpath.psi.JsonPathWildcardSegment getWildcardSegment() {
    return findChildByClass(fit.intellij.jsonpath.psi.JsonPathWildcardSegment.class);
  }

}
