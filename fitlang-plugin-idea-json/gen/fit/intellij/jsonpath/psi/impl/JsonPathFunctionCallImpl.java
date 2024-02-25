// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi.impl;

import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.extapi.psi.ASTWrapperPsiElement;

public class JsonPathFunctionCallImpl extends ASTWrapperPsiElement implements fit.intellij.jsonpath.psi.JsonPathFunctionCall {

  public JsonPathFunctionCallImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull fit.intellij.jsonpath.psi.JsonPathVisitor visitor) {
    visitor.visitFunctionCall(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.jsonpath.psi.JsonPathVisitor) accept((fit.intellij.jsonpath.psi.JsonPathVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public fit.intellij.jsonpath.psi.JsonPathFunctionArgsList getFunctionArgsList() {
    return findChildByClass(fit.intellij.jsonpath.psi.JsonPathFunctionArgsList.class);
  }

  @Override
  @NotNull
  public fit.intellij.jsonpath.psi.JsonPathId getId() {
    return findNotNullChildByClass(fit.intellij.jsonpath.psi.JsonPathId.class);
  }

}
