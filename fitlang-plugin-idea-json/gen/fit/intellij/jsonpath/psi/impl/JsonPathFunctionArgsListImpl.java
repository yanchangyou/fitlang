// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.extapi.psi.ASTWrapperPsiElement;

public class JsonPathFunctionArgsListImpl extends ASTWrapperPsiElement implements fit.intellij.jsonpath.psi.JsonPathFunctionArgsList {

  public JsonPathFunctionArgsListImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull fit.intellij.jsonpath.psi.JsonPathVisitor visitor) {
    visitor.visitFunctionArgsList(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.jsonpath.psi.JsonPathVisitor) accept((fit.intellij.jsonpath.psi.JsonPathVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<fit.intellij.jsonpath.psi.JsonPathExpression> getExpressionList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, fit.intellij.jsonpath.psi.JsonPathExpression.class);
  }

}
