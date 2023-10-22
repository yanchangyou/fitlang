// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.extension;

import com.intellij.psi.PsiElement;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.impl.JsonSchemaObject;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public interface JsonValidationHost {
  void error(final String error, final PsiElement holder, JsonErrorPriority priority);
  void error(final PsiElement newHolder, fit.jetbrains.jsonSchema.impl.JsonValidationError error);
  void error(final String error, final PsiElement holder,
             fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind fixableIssueKind,
             fit.jetbrains.jsonSchema.impl.JsonValidationError.IssueData data,
             JsonErrorPriority priority);

  void typeError(final @NotNull PsiElement value, @Nullable fit.jetbrains.jsonSchema.impl.JsonSchemaType currentType, final fit.jetbrains.jsonSchema.impl.JsonSchemaType @NotNull ... allowedTypes);

  fit.jetbrains.jsonSchema.impl.MatchResult resolve(fit.jetbrains.jsonSchema.impl.JsonSchemaObject schemaObject);

  @Nullable
  JsonValidationHost checkByMatchResult(fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter adapter, fit.jetbrains.jsonSchema.impl.MatchResult result, fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions options);

  boolean isValid();

  void checkObjectBySchemaRecordErrors(@NotNull JsonSchemaObject schema, @NotNull JsonValueAdapter object);

  void addErrorsFrom(JsonValidationHost otherHost);
}
