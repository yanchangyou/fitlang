/*
 * Copyright 2000-2017 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fit.jetbrains.jsonSchema.impl;

import com.intellij.codeInsight.completion.CompletionUtil;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReferenceContributor;
import com.intellij.psi.PsiReferenceRegistrar;
import com.intellij.psi.filters.ElementFilter;
import com.intellij.psi.filters.position.FilterPattern;
import com.intellij.util.ObjectUtils;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import fit.intellij.json.psi.JsonValue;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author Irina.Chernushina on 3/31/2016.
 */
public class JsonSchemaReferenceContributor extends PsiReferenceContributor {
  private static class Holder {
    private static final PsiElementPattern.Capture<fit.intellij.json.psi.JsonValue> REF_PATTERN = createPropertyValuePattern("$ref", true, false);
    private static final PsiElementPattern.Capture<fit.intellij.json.psi.JsonValue> REC_REF_PATTERN = createPropertyValuePattern("$recursiveRef", true, false);
    private static final PsiElementPattern.Capture<fit.intellij.json.psi.JsonValue> SCHEMA_PATTERN = createPropertyValuePattern("$schema", false, true);
    private static final PsiElementPattern.Capture<fit.intellij.json.psi.JsonStringLiteral> REQUIRED_PROP_PATTERN = createRequiredPropPattern();
  }
  @Override
  public void registerReferenceProviders(@NotNull PsiReferenceRegistrar registrar) {
    registrar.registerReferenceProvider(Holder.REF_PATTERN, new fit.jetbrains.jsonSchema.impl.JsonPointerReferenceProvider(false));
    registrar.registerReferenceProvider(Holder.REC_REF_PATTERN, new fit.jetbrains.jsonSchema.impl.JsonPointerReferenceProvider(false));
    registrar.registerReferenceProvider(Holder.SCHEMA_PATTERN, new JsonPointerReferenceProvider(true));
    registrar.registerReferenceProvider(Holder.REQUIRED_PROP_PATTERN, new JsonRequiredPropsReferenceProvider());
  }

  private static PsiElementPattern.Capture<fit.intellij.json.psi.JsonValue> createPropertyValuePattern(
    @SuppressWarnings("SameParameterValue") @NotNull final String propertyName, boolean schemaOnly, boolean rootOnly) {

    return PlatformPatterns.psiElement(fit.intellij.json.psi.JsonValue.class).and(new FilterPattern(new ElementFilter() {
      @Override
      public boolean isAcceptable(Object element, @Nullable PsiElement context) {
        if (element instanceof fit.intellij.json.psi.JsonValue) {
          final fit.intellij.json.psi.JsonValue value = (JsonValue) element;
          if (schemaOnly && !JsonSchemaService.isSchemaFile(CompletionUtil.getOriginalOrSelf(value.getContainingFile()))) return false;

          final fit.intellij.json.psi.JsonProperty property = ObjectUtils.tryCast(value.getParent(), fit.intellij.json.psi.JsonProperty.class);
          if (property != null && property.getValue() == element) {
            final PsiFile file = property.getContainingFile();
            if (rootOnly && (!(file instanceof fit.intellij.json.psi.JsonFile) || ((fit.intellij.json.psi.JsonFile)file).getTopLevelValue() != property.getParent())) return false;
            return propertyName.equals(property.getName());
          }
        }
        return false;
      }

      @Override
      public boolean isClassAcceptable(Class hintClass) {
        return true;
      }
    }));
  }

  private static PsiElementPattern.Capture<fit.intellij.json.psi.JsonStringLiteral> createRequiredPropPattern() {
    return PlatformPatterns.psiElement(fit.intellij.json.psi.JsonStringLiteral.class).and(new FilterPattern(new ElementFilter() {
      @Override
      public boolean isAcceptable(Object element, @Nullable PsiElement context) {
        if (!(element instanceof fit.intellij.json.psi.JsonStringLiteral)) return false;
        if (!JsonSchemaService.isSchemaFile(((fit.intellij.json.psi.JsonStringLiteral)element).getContainingFile())) return false;
        final PsiElement parent = ((fit.intellij.json.psi.JsonStringLiteral)element).getParent();
        if (!(parent instanceof fit.intellij.json.psi.JsonArray)) return false;
        PsiElement property = parent.getParent();
        if (!(property instanceof fit.intellij.json.psi.JsonProperty)) return false;
        return "required".equals(((fit.intellij.json.psi.JsonProperty)property).getName());
      }

      @Override
      public boolean isClassAcceptable(Class hintClass) {
        return true;
      }
    }));
  }
}
