// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.extapi.psi.ASTWrapperPsiElement;

public class JsonPathObjectPropertyImpl extends ASTWrapperPsiElement implements fit.intellij.jsonpath.psi.JsonPathObjectProperty {

  public JsonPathObjectPropertyImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull fit.intellij.jsonpath.psi.JsonPathVisitor visitor) {
    visitor.visitObjectProperty(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.jsonpath.psi.JsonPathVisitor) accept((fit.intellij.jsonpath.psi.JsonPathVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<fit.intellij.jsonpath.psi.JsonPathValue> getValueList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, fit.intellij.jsonpath.psi.JsonPathValue.class);
  }

}
