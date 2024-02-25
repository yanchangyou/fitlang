// This is a generated file. Not intended for manual editing.
package fit.intellij.json.psi.impl;

import fit.intellij.json.psi.JsonElementVisitor;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;

public class JsonNumberLiteralImpl extends JsonLiteralImpl implements fit.intellij.json.psi.JsonNumberLiteral {

  public JsonNumberLiteralImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull fit.intellij.json.psi.JsonElementVisitor visitor) {
    visitor.visitNumberLiteral(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.json.psi.JsonElementVisitor) accept((JsonElementVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  public double getValue() {
    return JsonPsiImplUtils.getValue(this);
  }

}
