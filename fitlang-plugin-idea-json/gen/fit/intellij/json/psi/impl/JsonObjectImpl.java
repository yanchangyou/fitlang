// This is a generated file. Not intended for manual editing.
package fit.intellij.json.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static fit.intellij.json.JsonElementTypes.*;
import fit.intellij.json.psi.*;
import com.intellij.navigation.ItemPresentation;

public class JsonObjectImpl extends JsonObjectMixin implements JsonObject {

  public JsonObjectImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull JsonElementVisitor visitor) {
    visitor.visitObject(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof JsonElementVisitor) accept((JsonElementVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<JsonProperty> getPropertyList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, JsonProperty.class);
  }

  @Override
  public @Nullable ItemPresentation getPresentation() {
    return JsonPsiImplUtils.getPresentation(this);
  }

}
