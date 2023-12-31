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
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;

public class JsonStringLiteralImpl extends JsonStringLiteralMixin implements JsonStringLiteral {

  public JsonStringLiteralImpl(ASTNode node) {
    super(node);
  }

  @Override
  public void accept(@NotNull JsonElementVisitor visitor) {
    visitor.visitStringLiteral(this);
  }

  @Override
  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof JsonElementVisitor) accept((JsonElementVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public JsonMyKeyword getMyKeyword() {
    return findChildByClass(JsonMyKeyword.class);
  }

  @Override
  public @NotNull List<Pair<TextRange, String>> getTextFragments() {
    return JsonPsiImplUtils.getTextFragments(this);
  }

  @Override
  public @NotNull String getValue() {
    return JsonPsiImplUtils.getValue(this);
  }

  @Override
  public boolean isPropertyName() {
    return JsonPsiImplUtils.isPropertyName(this);
  }

}
