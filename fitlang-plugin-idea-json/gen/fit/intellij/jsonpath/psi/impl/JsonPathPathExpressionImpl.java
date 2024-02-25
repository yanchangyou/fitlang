// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;

public class JsonPathPathExpressionImpl extends JsonPathExpressionImpl implements fit.intellij.jsonpath.psi.JsonPathPathExpression {

  public JsonPathPathExpressionImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull fit.intellij.jsonpath.psi.JsonPathVisitor visitor) {
    visitor.visitPathExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.jsonpath.psi.JsonPathVisitor) accept((fit.intellij.jsonpath.psi.JsonPathVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public fit.intellij.jsonpath.psi.JsonPathEvalSegment getEvalSegment() {
    return findChildByClass(fit.intellij.jsonpath.psi.JsonPathEvalSegment.class);
  }

  @Override
  @NotNull
  public List<fit.intellij.jsonpath.psi.JsonPathExpressionSegment> getExpressionSegmentList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, fit.intellij.jsonpath.psi.JsonPathExpressionSegment.class);
  }

  @Override
  @NotNull
  public List<fit.intellij.jsonpath.psi.JsonPathFunctionCall> getFunctionCallList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, fit.intellij.jsonpath.psi.JsonPathFunctionCall.class);
  }

  @Override
  @NotNull
  public List<fit.intellij.jsonpath.psi.JsonPathIdSegment> getIdSegmentList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, fit.intellij.jsonpath.psi.JsonPathIdSegment.class);
  }

  @Override
  @Nullable
  public fit.intellij.jsonpath.psi.JsonPathRootSegment getRootSegment() {
    return findChildByClass(fit.intellij.jsonpath.psi.JsonPathRootSegment.class);
  }

  @Override
  @NotNull
  public List<fit.intellij.jsonpath.psi.JsonPathWildcardSegment> getWildcardSegmentList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, fit.intellij.jsonpath.psi.JsonPathWildcardSegment.class);
  }

}
