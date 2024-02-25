// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;

public class JsonPathConditionalExpressionImpl extends JsonPathExpressionImpl implements fit.intellij.jsonpath.psi.JsonPathConditionalExpression {

  public JsonPathConditionalExpressionImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull fit.intellij.jsonpath.psi.JsonPathVisitor visitor) {
    visitor.visitConditionalExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.jsonpath.psi.JsonPathVisitor) accept((fit.intellij.jsonpath.psi.JsonPathVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public fit.intellij.jsonpath.psi.JsonPathBinaryConditionalOperator getBinaryConditionalOperator() {
    return findNotNullChildByClass(fit.intellij.jsonpath.psi.JsonPathBinaryConditionalOperator.class);
  }

  @Override
  @NotNull
  public List<fit.intellij.jsonpath.psi.JsonPathExpression> getExpressionList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, fit.intellij.jsonpath.psi.JsonPathExpression.class);
  }

}
