// This is a generated file. Not intended for manual editing.
package fit.intellij.json.psi.impl;

import fit.intellij.json.psi.JsonElementVisitor;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;

public abstract class JsonValueImpl extends JsonElementImpl implements fit.intellij.json.psi.JsonValue {

  public JsonValueImpl(@NotNull ASTNode node) {
    super(node);
  }

  public void accept(@NotNull fit.intellij.json.psi.JsonElementVisitor visitor) {
    visitor.visitValue(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.json.psi.JsonElementVisitor) accept((JsonElementVisitor)visitor);
    else super.accept(visitor);
  }

}
