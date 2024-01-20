// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl;

import fit.intellij.json.pointer.JsonPointerPosition;
import com.intellij.lang.injection.MultiHostRegistrar;
import com.intellij.psi.PsiElement;
import com.intellij.util.ThreeState;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import fit.intellij.json.psi.JsonStringLiteral;
import org.intellij.lang.regexp.ecmascript.EcmaScriptRegexpLanguage;
import org.jetbrains.annotations.NotNull;

public class JsonSchemaRegexInjector extends JsonSchemaInjectorBase {
  @Override
  public void getLanguagesToInject(@NotNull MultiHostRegistrar registrar, @NotNull PsiElement context) {
    if (!(context instanceof fit.intellij.json.psi.JsonStringLiteral)) return;
    if (!JsonSchemaService.isSchemaFile(context.getContainingFile())) return;
    fit.jetbrains.jsonSchema.impl.JsonOriginalPsiWalker walker = JsonOriginalPsiWalker.INSTANCE;
    ThreeState isName = walker.isName(context);
    JsonPointerPosition position = walker.findPosition(context, isName == ThreeState.NO);
    if (position == null || position.isEmpty()) return;
    if (isName == ThreeState.YES) {
      if ("patternProperties".equals(position.getLastName())) {
        if (isNestedInPropertiesList(position)) return;
        injectForHost(registrar, (fit.intellij.json.psi.JsonStringLiteral)context, EcmaScriptRegexpLanguage.INSTANCE);
      }
    }
    else if (isName == ThreeState.NO) {
      if ("pattern".equals(position.getLastName())) {
        if (isNestedInPropertiesList(position)) return;
        injectForHost(registrar, (JsonStringLiteral)context, EcmaScriptRegexpLanguage.INSTANCE);
      }
    }
  }

  private static boolean isNestedInPropertiesList(JsonPointerPosition position) {
    final JsonPointerPosition skipped = position.trimTail(1);
    return skipped != null && "properties".equals(skipped.getLastName());
  }
}
