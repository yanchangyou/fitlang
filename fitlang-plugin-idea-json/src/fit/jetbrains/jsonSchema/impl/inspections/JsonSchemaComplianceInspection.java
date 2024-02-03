// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl.inspections;

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.codeInspection.options.OptPane;
import fit.intellij.json.JsonBundle;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import fit.intellij.json.psi.JsonElementVisitor;
import fit.intellij.json.psi.JsonValue;
import fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker;
import fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions;
import fit.jetbrains.jsonSchema.impl.JsonSchemaComplianceChecker;
import fit.jetbrains.jsonSchema.impl.JsonSchemaObject;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static com.intellij.codeInspection.options.OptPane.checkbox;
import static com.intellij.codeInspection.options.OptPane.pane;

public class JsonSchemaComplianceInspection extends JsonSchemaBasedInspectionBase {
  public boolean myCaseInsensitiveEnum = false;

  @Override
  protected PsiElementVisitor doBuildVisitor(@NotNull JsonValue root, @Nullable fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema, @NotNull JsonSchemaService service,
                                             @NotNull ProblemsHolder holder,
                                             @NotNull LocalInspectionToolSession session) {
    if (schema == null) return PsiElementVisitor.EMPTY_VISITOR;
    fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions options = new fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions(myCaseInsensitiveEnum);

    return new JsonElementVisitor() {
      @Override
      public void visitElement(@NotNull PsiElement element) {
        if (element == root) {
          // perform this only for the root element, because the checker traverses the hierarchy itself
          annotate(element, schema, holder, session, options);
        }
        super.visitElement(element);
      }
    };
  }

  @Override
  public @NotNull OptPane getOptionsPane() {
    return pane(
      checkbox("myCaseInsensitiveEnum", JsonBundle.message("json.schema.inspection.case.insensitive.enum")));
  }

  private static void annotate(@NotNull PsiElement element,
                               @NotNull JsonSchemaObject rootSchema,
                               @NotNull ProblemsHolder holder,
                               @NotNull LocalInspectionToolSession session,
                               JsonComplianceCheckerOptions options) {
    final fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker walker = JsonLikePsiWalker.getWalker(element, rootSchema);
    if (walker == null) return;
    new JsonSchemaComplianceChecker(rootSchema, holder, walker, session, options).annotate(element);
  }
}
