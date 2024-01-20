// This is a generated file. Not intended for manual editing.
package fit.intellij.json.psi.impl;

import java.util.List;

import fit.intellij.json.psi.JsonValue;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.navigation.ItemPresentation;

public class JsonArrayImpl extends JsonContainerImpl implements fit.intellij.json.psi.JsonArray {

  public JsonArrayImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull fit.intellij.json.psi.JsonElementVisitor visitor) {
    visitor.visitArray(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof fit.intellij.json.psi.JsonElementVisitor) accept((fit.intellij.json.psi.JsonElementVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<fit.intellij.json.psi.JsonValue> getValueList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, JsonValue.class);
  }

  @Override
  public @Nullable ItemPresentation getPresentation() {
    return JsonPsiImplUtils.getPresentation(this);
  }

}
