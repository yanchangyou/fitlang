// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl.fixes;

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import fit.intellij.json.JsonBundle;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import fit.jetbrains.jsonSchema.extension.JsonLikeSyntaxAdapter;
import fit.jetbrains.jsonSchema.impl.JsonValidationError;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

public class RemoveProhibitedPropertyFix implements LocalQuickFix {
  @SafeFieldForPreview
  private final fit.jetbrains.jsonSchema.impl.JsonValidationError.ProhibitedPropertyIssueData myData;
  @SafeFieldForPreview
  private final fit.jetbrains.jsonSchema.extension.JsonLikeSyntaxAdapter myQuickFixAdapter;

  public RemoveProhibitedPropertyFix(JsonValidationError.ProhibitedPropertyIssueData data,
                                     JsonLikeSyntaxAdapter quickFixAdapter) {
    myData = data;
    myQuickFixAdapter = quickFixAdapter;
  }

  @Nls(capitalization = Nls.Capitalization.Sentence)
  @NotNull
  @Override
  public String getFamilyName() {
    return JsonBundle.message("remove.prohibited.property");
  }

  @Nls(capitalization = Nls.Capitalization.Sentence)
  @NotNull
  @Override
  public String getName() {
    return getFamilyName() + " '" + myData.propertyName + "'";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    PsiElement element = descriptor.getPsiElement();
    assert myData.propertyName.equals(myQuickFixAdapter.getPropertyName(element));
    PsiElement forward = PsiTreeUtil.skipWhitespacesForward(element);
    element.delete();
    myQuickFixAdapter.removeIfComma(forward);
  }
}
