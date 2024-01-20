// This is a generated file. Not intended for manual editing.
package fit.intellij.json.psi.impl;

import fit.intellij.json.JsonElementTypes;
import fit.intellij.json.psi.JsonElementVisitor;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;

public class JsonReferenceExpressionImpl extends JsonReferenceLiteralMixin implements fit.intellij.json.psi.JsonReferenceExpression {

  public JsonReferenceExpressionImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull fit.intellij.json.psi.JsonElementVisitor visitor) {
    visitor.visitReferenceExpression(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.json.psi.JsonElementVisitor) accept((JsonElementVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public PsiElement getIdentifier() {
    return findNotNullChildByType(JsonElementTypes.IDENTIFIER);
  }

}
