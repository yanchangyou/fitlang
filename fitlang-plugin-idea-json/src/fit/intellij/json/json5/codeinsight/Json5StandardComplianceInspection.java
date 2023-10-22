// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.json5.codeinsight;

import com.intellij.codeInspection.ProblemsHolder;
import fit.intellij.json.JsonBundle;
import fit.intellij.json.JsonDialectUtil;
import fit.intellij.json.codeinsight.JsonStandardComplianceInspection;
import fit.intellij.json.json5.Json5Language;
import fit.intellij.json.psi.JsonLiteral;
import fit.intellij.json.psi.JsonPsiUtil;
import fit.intellij.json.psi.JsonReferenceExpression;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class Json5StandardComplianceInspection extends JsonStandardComplianceInspection {

  @Override
  @NotNull
  public String getDisplayName() {
    return JsonBundle.message("inspection.compliance5.name");
  }

  @NotNull
  @Override
  public PsiElementVisitor buildVisitor(@NotNull final ProblemsHolder holder, boolean isOnTheFly) {
    if (!(JsonDialectUtil.getLanguage(holder.getFile()) instanceof Json5Language)) return PsiElementVisitor.EMPTY_VISITOR;
    return new StandardJson5ValidatingElementVisitor(holder);
  }

  @Override
  public JComponent createOptionsPanel() {
    return null;
  }

  private class StandardJson5ValidatingElementVisitor extends StandardJsonValidatingElementVisitor {
    StandardJson5ValidatingElementVisitor(ProblemsHolder holder) {
      super(holder);
    }

    @Override
    protected boolean allowComments() {
      return true;
    }

    @Override
    protected boolean allowSingleQuotes() {
      return true;
    }

    @Override
    protected boolean allowIdentifierPropertyNames() {
      return true;
    }

    @Override
    protected boolean allowTrailingCommas() {
      return true;
    }

    @Override
    protected boolean allowNanInfinity() {
      return true;
    }

    @Override
    protected boolean isValidPropertyName(@NotNull PsiElement literal) {
      if (literal instanceof JsonLiteral) {
        String textWithoutHostEscaping = JsonPsiUtil.getElementTextWithoutHostEscaping(literal);
        return textWithoutHostEscaping.startsWith("\"") || textWithoutHostEscaping.startsWith("'");
      }
      if (literal instanceof JsonReferenceExpression) {
        return StringUtil.isJavaIdentifier(literal.getText());
      }
      return false;
    }
  }
}