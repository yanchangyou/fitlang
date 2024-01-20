// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
package fit.intellij.json;

import fit.intellij.json.pointer.JsonPointerPosition;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiUtilCore;
import com.intellij.util.ObjectUtils;
import com.intellij.util.ThreeState;
import com.intellij.util.containers.ContainerUtil;
import fit.intellij.json.psi.JsonObject;
import fit.intellij.json.psi.JsonProperty;
import fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import fit.jetbrains.jsonSchema.impl.JsonSchemaObject;
import fit.jetbrains.jsonSchema.impl.JsonSchemaResolver;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

public abstract class JsonSchemaSpellcheckerClient {
  protected abstract @NotNull PsiElement getElement();

  protected abstract @Nullable String getValue();

  public boolean matchesNameFromSchema() {
    final VirtualFile file = PsiUtilCore.getVirtualFile(getElement());
    if (file == null) return false;

    Project project = getElement().getProject();
    final fit.jetbrains.jsonSchema.ide.JsonSchemaService service = fit.jetbrains.jsonSchema.ide.JsonSchemaService.Impl.get(project);
    if (!service.isApplicableToFile(file)) return false;
    final fit.jetbrains.jsonSchema.impl.JsonSchemaObject rootSchema = service.getSchemaObject(getElement().getContainingFile());
    if (rootSchema == null) return false;
    if (isXIntellijInjection(service, rootSchema)) return true;

    String value = getValue();
    if (StringUtil.isEmpty(value)) return false;

    fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker walker = JsonLikePsiWalker.getWalker(getElement(), rootSchema);
    if (walker == null) return false;
    final PsiElement checkable = walker.findElementToCheck(getElement());
    if (checkable == null) return false;
    final ThreeState isName = walker.isName(checkable);
    final JsonPointerPosition position = walker.findPosition(checkable, isName == ThreeState.NO);
    if (position == null || position.isEmpty() && isName == ThreeState.NO) return false;

    final Collection<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> schemas = new JsonSchemaResolver(project, rootSchema, position).resolve();
    if (schemas.isEmpty()) return false;

    return schemas.stream().anyMatch(s -> {
      if (s.getProperties().containsKey(value) || s.getMatchingPatternPropertySchema(value) != null) {
        return true;
      }
      return ContainerUtil.notNullize(s.getEnum()).stream().anyMatch(e -> e instanceof String && StringUtil.unquoteString((String)e).equals(value));
    });
  }

  protected boolean isXIntellijInjection(@NotNull JsonSchemaService service, @NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject rootSchema) {
    if (service.isSchemaFile(rootSchema)) {
      fit.intellij.json.psi.JsonProperty property = ObjectUtils.tryCast(getElement().getParent(), fit.intellij.json.psi.JsonProperty.class);
      if (property != null) {
        if (fit.jetbrains.jsonSchema.impl.JsonSchemaObject.X_INTELLIJ_LANGUAGE_INJECTION.equals(property.getName())) {
          return true;
        }
        if ("language".equals(property.getName())) {
          PsiElement parent = property.getParent();
          if (parent instanceof JsonObject) {
            PsiElement grandParent = parent.getParent();
            if (grandParent instanceof fit.intellij.json.psi.JsonProperty && JsonSchemaObject.X_INTELLIJ_LANGUAGE_INJECTION.equals(((JsonProperty)grandParent).getName())) {
              return true;
            }
          }
        }
      }
    }
    return false;
  }
}
