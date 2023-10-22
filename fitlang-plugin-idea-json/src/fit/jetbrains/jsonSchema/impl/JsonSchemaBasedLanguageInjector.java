// Copyright 2000-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl;

import fit.intellij.json.pointer.JsonPointerPosition;
import fit.intellij.json.psi.JsonStringLiteral;
import com.intellij.lang.Language;
import com.intellij.lang.injection.MultiHostRegistrar;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.ThreeState;
import fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

public class JsonSchemaBasedLanguageInjector extends JsonSchemaInjectorBase {
  @Override
  public void getLanguagesToInject(@NotNull MultiHostRegistrar registrar, @NotNull PsiElement context) {
    if (!(context instanceof JsonStringLiteral)) return;
    Language language = getLanguageToInject(context);
    if (language == null) return;
    injectForHost(registrar, (JsonStringLiteral)context, language);
  }

  @Nullable
  public static Language getLanguageToInject(@NotNull PsiElement context) {
    Project project = context.getProject();
    PsiFile containingFile = context.getContainingFile();
    JsonSchemaObject schemaObject = JsonSchemaService.Impl.get(project).getSchemaObject(containingFile);
    if (schemaObject == null) return null;
    fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker walker = JsonLikePsiWalker.getWalker(context, schemaObject);
    if (walker == null || walker.isName(context) != ThreeState.NO) return null;
    final JsonPointerPosition position = walker.findPosition(context, true);
    if (position == null || position.isEmpty()) return null;
    final Collection<JsonSchemaObject> schemas = new JsonSchemaResolver(project, schemaObject, position).resolve();
    for (JsonSchemaObject schema : schemas) {
      String injection = schema.getLanguageInjection();
      if (injection != null) {
        Language language = Language.findLanguageByID(injection);
        if (language != null) return language;
      }
    }
    return null;
  }
}
