// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl;

import com.intellij.psi.ElementManipulators;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiReferenceProvider;
import com.intellij.util.ProcessingContext;
import fit.intellij.json.psi.JsonObject;
import fit.intellij.json.psi.JsonProperty;
import fit.intellij.json.psi.JsonStringLiteral;
import fit.intellij.json.psi.JsonValue;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Optional;

public class JsonRequiredPropsReferenceProvider extends PsiReferenceProvider {
  @Override
  public PsiReference @NotNull [] getReferencesByElement(@NotNull PsiElement element, @NotNull ProcessingContext context) {
    return new PsiReference[] {new JsonRequiredPropReference((fit.intellij.json.psi.JsonStringLiteral)element)};
  }

  @Nullable
  public static fit.intellij.json.psi.JsonObject findPropertiesObject(PsiElement element) {
    PsiElement parent = getParentSafe(getParentSafe(getParentSafe(element)));
    if (!(parent instanceof fit.intellij.json.psi.JsonObject)) return null;
    Optional<fit.intellij.json.psi.JsonProperty> propertiesProp =
      ((fit.intellij.json.psi.JsonObject)parent).getPropertyList().stream().filter(p -> "properties".equals(p.getName())).findFirst();
    if (propertiesProp.isPresent()) {
      JsonValue value = propertiesProp.get().getValue();
      if (value instanceof fit.intellij.json.psi.JsonObject) {
        return (fit.intellij.json.psi.JsonObject)value;
      }
    }
    return null;
  }

  private static PsiElement getParentSafe(@Nullable PsiElement element) {
    return element == null ? null : element.getParent();
  }

  private static class JsonRequiredPropReference extends JsonSchemaBaseReference<fit.intellij.json.psi.JsonStringLiteral> {
    JsonRequiredPropReference(JsonStringLiteral element) {
      super(element, ElementManipulators.getValueTextRange(element));
    }

    @Nullable
    @Override
    public PsiElement resolveInner() {
      JsonObject propertiesObject = findPropertiesObject(getElement());
      if (propertiesObject != null) {
        String name = getElement().getValue();
        for (JsonProperty property : propertiesObject.getPropertyList()) {
          if (name.equals(property.getName())) return property;
        }
      }
      return null;
    }
  }
}
