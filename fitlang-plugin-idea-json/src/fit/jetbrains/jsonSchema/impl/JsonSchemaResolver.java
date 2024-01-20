// Copyright 2000-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl;

import fit.intellij.json.pointer.JsonPointerPosition;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.util.containers.ContainerUtil;
import fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker;
import fit.jetbrains.jsonSchema.extension.adapters.JsonArrayValueAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonObjectValueAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonPropertyAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.ide.JsonSchemaService;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

public class JsonSchemaResolver {
  @NotNull private final Project myProject;
  @NotNull private final fit.jetbrains.jsonSchema.impl.JsonSchemaObject mySchema;
  @NotNull private final JsonPointerPosition myPosition;

  public JsonSchemaResolver(@NotNull Project project,
                            @NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                            @NotNull JsonPointerPosition position) {
    myProject = project;
    mySchema = schema;
    myPosition = position;
  }

  public JsonSchemaResolver(@NotNull Project project, @NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema) {
    myProject = project;
    mySchema = schema;
    myPosition = new JsonPointerPosition();
  }

  public fit.jetbrains.jsonSchema.impl.MatchResult detailedResolve() {
    final JsonSchemaTreeNode node = JsonSchemaVariantsTreeBuilder.buildTree(myProject, mySchema, myPosition, false);
    return fit.jetbrains.jsonSchema.impl.MatchResult.create(node);
  }

  @NotNull
  public Collection<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> resolve() {
    final fit.jetbrains.jsonSchema.impl.MatchResult result = detailedResolve();
    final List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> list = new LinkedList<>();
    list.addAll(result.mySchemas);
    for (Collection<? extends fit.jetbrains.jsonSchema.impl.JsonSchemaObject> myExcludingSchema : result.myExcludingSchemas) {
      list.addAll(myExcludingSchema);
    }
    return list;
  }

  @Nullable
  public PsiElement findNavigationTarget(@Nullable final PsiElement element) {
    final JsonSchemaTreeNode node = JsonSchemaVariantsTreeBuilder
      .buildTree(myProject, mySchema, myPosition, true);
    final fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema = selectSchema(node, element, myPosition.isEmpty());
    if (schema == null) return null;
    VirtualFile file = JsonSchemaService.Impl.get(myProject).resolveSchemaFile(schema);
    if (file == null) return null;
    PsiFile psiFile = PsiManager.getInstance(myProject).findFile(file);
    if (psiFile == null) return null;
    JsonLikePsiWalker walker = JsonLikePsiWalker.getWalker(psiFile, schema);
    return walker == null ? null : resolvePosition(walker, psiFile, JsonPointerPosition.parsePointer(schema.getPointer()));
  }

  @Nullable
  private static PsiElement resolvePosition(@NotNull JsonLikePsiWalker walker,
                                            @Nullable PsiElement element,
                                            @NotNull JsonPointerPosition position) {
    PsiElement psiElement = element instanceof PsiFile ? ContainerUtil.getFirstItem(walker.getRoots((PsiFile)element)) : element;
    if (psiElement == null) return null;
    JsonValueAdapter value = walker.createValueAdapter(psiElement);
    while (position != null && !position.isEmpty()) {
      if (value instanceof JsonObjectValueAdapter) {
        String name = position.getFirstName();
        if (name == null) return null;
        JsonPropertyAdapter property = findProperty((JsonObjectValueAdapter)value, name);
        if (property != null) {
          value = getValue(property);
          if (value == null) return null;
        }
        else {
          JsonPropertyAdapter props = findProperty((JsonObjectValueAdapter)value, fit.jetbrains.jsonSchema.impl.JsonSchemaObject.PROPERTIES);
          if (props != null) {
            value = getValue(props);
            continue;
          }

          JsonPropertyAdapter defs = findProperty((JsonObjectValueAdapter)value, fit.jetbrains.jsonSchema.impl.JsonSchemaObject.DEFINITIONS);
          if (defs != null) {
            value = getValue(defs);
            continue;
          }

          JsonPropertyAdapter defs9 = findProperty((JsonObjectValueAdapter)value, fit.jetbrains.jsonSchema.impl.JsonSchemaObject.DEFINITIONS_v9);
          if (defs9 != null) {
            value = getValue(defs9);
            continue;
          }
          return null;
        }
      }
      else if (value instanceof JsonArrayValueAdapter) {
        int index = position.getFirstIndex();
        if (index >= 0) {
          List<JsonValueAdapter> values = ((JsonArrayValueAdapter)value).getElements();
          if (values.size() > index) {
            value = values.get(index);
          }
          else {
            return null;
          }
        }
      }
      position = position.skip(1);
    }
    if (value == null) {
      return null;
    }

    PsiElement delegate = value.getDelegate();
    PsiElement propertyNameElement = walker.getPropertyNameElement(delegate.getParent());
    return propertyNameElement == null ? delegate : propertyNameElement;
  }

  @Nullable
  private static JsonValueAdapter getValue(@NotNull JsonPropertyAdapter property) {
    Collection<JsonValueAdapter> values = property.getValues();
    return values.size() == 1 ? values.iterator().next() : null;
  }

  @Nullable
  private static JsonPropertyAdapter findProperty(@NotNull JsonObjectValueAdapter value, @NotNull String name) {
    List<JsonPropertyAdapter> list = value.getPropertyList();
    return ContainerUtil.find(list, p -> name.equals(p.getName()));
  }

  @Nullable
  private fit.jetbrains.jsonSchema.impl.JsonSchemaObject selectSchema(@NotNull final JsonSchemaTreeNode resolveRoot,
                                                                      @Nullable final PsiElement element,
                                                                      boolean topLevelSchema) {
    final fit.jetbrains.jsonSchema.impl.MatchResult matchResult = fit.jetbrains.jsonSchema.impl.MatchResult.create(resolveRoot);
    List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> schemas = new ArrayList<>(matchResult.mySchemas);
    schemas.addAll(matchResult.myExcludingSchemas.stream().flatMap(Collection::stream).toList());

    final fit.jetbrains.jsonSchema.impl.JsonSchemaObject firstSchema = getFirstValidSchema(schemas);
    if (element == null || schemas.size() == 1 || firstSchema == null) {
      return firstSchema;
    }
    // actually we pass any schema here
    final JsonLikePsiWalker walker = JsonLikePsiWalker.getWalker(element, firstSchema);
    JsonValueAdapter adapter;
    if (walker == null || (adapter = walker.createValueAdapter(element)) == null) return null;

    final JsonValueAdapter parentAdapter;
    if (topLevelSchema) {
      parentAdapter = null;
    } else {
      final PsiElement parentValue = walker.getParentContainer(element);
      if (parentValue == null || (parentAdapter = walker.createValueAdapter(parentValue)) == null) return null;
    }

    final Ref<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> schemaRef = new Ref<>();
    MatchResult.iterateTree(resolveRoot, node -> {
      final JsonSchemaTreeNode parent = node.getParent();
      if (node.getSchema() == null || parentAdapter != null && parent != null && parent.isNothing()) return true;
      if (!isCorrect(adapter, node.getSchema())) return true;
      if (parentAdapter == null ||
          parent == null ||
          parent.getSchema() == null ||
          parent.isAny() ||
          isCorrect(parentAdapter, parent.getSchema())) {
        schemaRef.set(node.getSchema());
        return false;
      }
      return true;
    });
    return schemaRef.get();
  }

  @Nullable
  private static fit.jetbrains.jsonSchema.impl.JsonSchemaObject getFirstValidSchema(List<fit.jetbrains.jsonSchema.impl.JsonSchemaObject> schemas) {
    return schemas.stream().findFirst().orElse(null);
  }

  private boolean isCorrect(@NotNull final JsonValueAdapter value, @NotNull final JsonSchemaObject schema) {
    final fit.jetbrains.jsonSchema.impl.JsonSchemaType type = JsonSchemaType.getType(value);
    if (type == null) return true;
    if (!JsonSchemaAnnotatorChecker.areSchemaTypesCompatible(schema, type)) return false;
    final JsonSchemaAnnotatorChecker checker = new JsonSchemaAnnotatorChecker(myProject, JsonComplianceCheckerOptions.RELAX_ENUM_CHECK);
    checker.checkByScheme(value, schema);
    return checker.isCorrect();
  }
}
