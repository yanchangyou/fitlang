// This is a generated file. Not intended for manual editing.
package fit.intellij.json.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import fit.intellij.json.psi.JsonElementVisitor;
import fit.intellij.json.psi.JsonMyKeyword;
import org.jetbrains.annotations.NotNull;

public class JsonMyKeywordImpl extends ASTWrapperPsiElement implements JsonMyKeyword {

    public JsonMyKeywordImpl(@NotNull ASTNode node) {
        super(node);
    }

    public void accept(@NotNull JsonElementVisitor visitor) {
        visitor.visitMyKeyword(this);
    }

    @Override
    public void accept(@NotNull PsiElementVisitor visitor) {
        if (visitor instanceof JsonElementVisitor) accept((JsonElementVisitor) visitor);
        else super.accept(visitor);
    }

}
