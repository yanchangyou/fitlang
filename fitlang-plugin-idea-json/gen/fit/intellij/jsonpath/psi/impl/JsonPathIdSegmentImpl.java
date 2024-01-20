// This is a generated file. Not intended for manual editing.
package fit.intellij.jsonpath.psi.impl;

import fit.intellij.jsonpath.psi.JsonPathId;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.extapi.psi.ASTWrapperPsiElement;

public class JsonPathIdSegmentImpl extends ASTWrapperPsiElement implements fit.intellij.jsonpath.psi.JsonPathIdSegment {

  public JsonPathIdSegmentImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull fit.intellij.jsonpath.psi.JsonPathVisitor visitor) {
    visitor.visitIdSegment(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.jsonpath.psi.JsonPathVisitor) accept((fit.intellij.jsonpath.psi.JsonPathVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public fit.intellij.jsonpath.psi.JsonPathId getId() {
    return findNotNullChildByClass(JsonPathId.class);
  }

}
