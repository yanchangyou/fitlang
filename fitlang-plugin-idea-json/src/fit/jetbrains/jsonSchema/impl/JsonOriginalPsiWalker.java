// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl;

import com.intellij.codeInsight.completion.CompletionUtil;
import fit.intellij.json.JsonDialectUtil;
import fit.intellij.json.pointer.JsonPointerPosition;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.ThreeState;
import com.intellij.util.containers.ContainerUtil;
import fit.intellij.json.psi.JsonElementGenerator;
import fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker;
import fit.jetbrains.jsonSchema.extension.adapters.JsonPropertyAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.impl.adapters.JsonJsonPropertyAdapter;
import fit.intellij.json.JsonElementTypes;
import fit.jetbrains.jsonSchema.extension.JsonLikeSyntaxAdapter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class JsonOriginalPsiWalker implements JsonLikePsiWalker {
  public static final JsonOriginalPsiWalker INSTANCE = new JsonOriginalPsiWalker();

  public boolean handles(@NotNull PsiElement element) {
    PsiElement parent = element.getParent();
    return element instanceof fit.intellij.json.psi.JsonFile && JsonDialectUtil.isStandardJson(element)
           || parent != null && (element instanceof fit.intellij.json.psi.JsonElement || element instanceof LeafPsiElement && parent instanceof fit.intellij.json.psi.JsonElement)
             && JsonDialectUtil.isStandardJson(CompletionUtil.getOriginalOrSelf(parent));
  }

  @Override
  public ThreeState isName(PsiElement element) {
    final PsiElement parent = element.getParent();
    if (parent instanceof fit.intellij.json.psi.JsonObject) {
      return ThreeState.YES;
    } else if (parent instanceof fit.intellij.json.psi.JsonProperty) {
      return PsiTreeUtil.isAncestor(((fit.intellij.json.psi.JsonProperty)parent).getNameElement(), element, false) ? ThreeState.YES : ThreeState.NO;
    }
    return ThreeState.NO;
  }

  @Override
  public boolean isPropertyWithValue(@NotNull PsiElement element) {
    if (element instanceof fit.intellij.json.psi.JsonStringLiteral || element instanceof fit.intellij.json.psi.JsonReferenceExpression) {
      final PsiElement parent = element.getParent();
      if (!(parent instanceof fit.intellij.json.psi.JsonProperty) || ((fit.intellij.json.psi.JsonProperty)parent).getNameElement() != element) return false;
      element = parent;
    }
    return element instanceof fit.intellij.json.psi.JsonProperty && ((fit.intellij.json.psi.JsonProperty)element).getValue() != null;
  }

  @Override
  public PsiElement findElementToCheck(@NotNull PsiElement element) {
    PsiElement current = element;
    while (current != null && !(current instanceof PsiFile)) {
      if (current instanceof fit.intellij.json.psi.JsonValue || current instanceof fit.intellij.json.psi.JsonProperty) {
        return current;
      }
      current = current.getParent();
    }
    return null;
  }

  @Nullable
  @Override
  public JsonPointerPosition findPosition(@NotNull PsiElement element, boolean forceLastTransition) {
    JsonPointerPosition pos = new JsonPointerPosition();
    PsiElement current = element;
    while (! (current instanceof PsiFile)) {
      final PsiElement position = current;
      current = current.getParent();
      if (current instanceof fit.intellij.json.psi.JsonArray array) {
        final List<fit.intellij.json.psi.JsonValue> list = array.getValueList();
        int idx = -1;
        for (int i = 0; i < list.size(); i++) {
          final fit.intellij.json.psi.JsonValue value = list.get(i);
          if (value.equals(position)) {
            idx = i;
            break;
          }
        }
        pos.addPrecedingStep(idx);
      } else if (current instanceof fit.intellij.json.psi.JsonProperty) {
        final String propertyName = ((fit.intellij.json.psi.JsonProperty)current).getName();
        current = current.getParent();
        if (!(current instanceof fit.intellij.json.psi.JsonObject)) return null;//incorrect syntax?
        // if either value or not first in the chain - needed for completion variant
        if (position != element || forceLastTransition) {
          pos.addPrecedingStep(propertyName);
        }
      } else if (current instanceof fit.intellij.json.psi.JsonObject && position instanceof fit.intellij.json.psi.JsonProperty) {
        // if either value or not first in the chain - needed for completion variant
        if (position != element || forceLastTransition) {
          final String propertyName = ((fit.intellij.json.psi.JsonProperty)position).getName();
          pos.addPrecedingStep(propertyName);
        }
      } else if (current instanceof PsiFile) {
        break;
      } else {
        return null;//something went wrong
      }
    }
    return pos;
  }

  @Override
  public boolean requiresNameQuotes() {
    return true;
  }

  @Override
  public boolean allowsSingleQuotes() {
    return false;
  }

  @Override
  public boolean hasMissingCommaAfter(@NotNull PsiElement element) {
    PsiElement current = element instanceof fit.intellij.json.psi.JsonProperty ? element : PsiTreeUtil.getParentOfType(element, fit.intellij.json.psi.JsonProperty.class);
    while (current != null && current.getNode().getElementType() != fit.intellij.json.JsonElementTypes.COMMA) {
      current = current.getNextSibling();
    }
    int commaOffset = current == null ? Integer.MAX_VALUE : current.getTextRange().getStartOffset();
    final int offset = element.getTextRange().getStartOffset();
    final fit.intellij.json.psi.JsonObject object = PsiTreeUtil.getParentOfType(element, fit.intellij.json.psi.JsonObject.class);
    if (object != null) {
      for (fit.intellij.json.psi.JsonProperty property : object.getPropertyList()) {
        final int pOffset = property.getTextRange().getStartOffset();
        if (pOffset >= offset && !PsiTreeUtil.isAncestor(property, element, false)) {
          return pOffset < commaOffset;
        }
      }
    }
    return false;
  }

  @Override
  public Set<String> getPropertyNamesOfParentObject(@NotNull PsiElement originalPosition, PsiElement computedPosition) {
    final fit.intellij.json.psi.JsonObject object = PsiTreeUtil.getParentOfType(originalPosition, fit.intellij.json.psi.JsonObject.class);
    if (object != null) {
      return object.getPropertyList().stream()
        .filter(p -> !requiresNameQuotes() || p.getNameElement() instanceof fit.intellij.json.psi.JsonStringLiteral)
        .map(p -> StringUtil.unquoteString(p.getName())).collect(Collectors.toSet());
    }
    return Collections.emptySet();
  }

  @Override
  public JsonPropertyAdapter getParentPropertyAdapter(@NotNull PsiElement element) {
    final fit.intellij.json.psi.JsonProperty property = PsiTreeUtil.getParentOfType(element, fit.intellij.json.psi.JsonProperty.class, false);
    if (property == null) return null;
    return new JsonJsonPropertyAdapter(property);
  }

  @Override
  public boolean isTopJsonElement(@NotNull PsiElement element) {
    return element instanceof PsiFile;
  }

  @Nullable
  @Override
  public JsonValueAdapter createValueAdapter(@NotNull PsiElement element) {
    return element instanceof fit.intellij.json.psi.JsonValue ? JsonJsonPropertyAdapter.createAdapterByType((fit.intellij.json.psi.JsonValue)element) : null;
  }

  @Override
  public fit.jetbrains.jsonSchema.extension.JsonLikeSyntaxAdapter getSyntaxAdapter(Project project) {
    return new JsonLikeSyntaxAdapter() {
      private final fit.intellij.json.psi.JsonElementGenerator myGenerator = new JsonElementGenerator(project);
      @Nullable
      @Override
      public PsiElement getPropertyValue(PsiElement property) {
        assert property instanceof fit.intellij.json.psi.JsonProperty;
        return ((fit.intellij.json.psi.JsonProperty)property).getValue();
      }

      @NotNull
      @Override
      public String getPropertyName(PsiElement property) {
        assert property instanceof fit.intellij.json.psi.JsonProperty;
        return ((fit.intellij.json.psi.JsonProperty)property).getName();
      }

      @NotNull
      @Override
      public PsiElement createProperty(@NotNull String name, @NotNull String value, PsiElement element) {
        return myGenerator.createProperty(name, value);
      }

      @Override
      public boolean ensureComma(PsiElement self, PsiElement newElement) {
        if (newElement instanceof fit.intellij.json.psi.JsonProperty && self instanceof fit.intellij.json.psi.JsonProperty) {
          self.getParent().addAfter(myGenerator.createComma(), self);
          return true;
        }
        return false;
      }

      @Override
      public void removeIfComma(PsiElement forward) {
        if (forward instanceof LeafPsiElement && ((LeafPsiElement)forward).getElementType() == JsonElementTypes.COMMA) {
          forward.delete();
        }
      }

      @Override
      public boolean fixWhitespaceBefore(PsiElement initialElement, PsiElement element) {
        return true;
      }

      @NotNull
      @Override
      public String getDefaultValueFromType(@Nullable JsonSchemaType type) {
        return type == null ? "" : type.getDefaultValue();
      }

      @Override
      public PsiElement adjustNewProperty(PsiElement element) {
        return element;
      }

      @Override
      public PsiElement adjustPropertyAnchor(LeafPsiElement element) {
        throw new IncorrectOperationException("Shouldn't use leafs for insertion in pure JSON!");
      }
    };
  }

  @Nullable
  @Override
  public PsiElement getParentContainer(PsiElement element) {
    return PsiTreeUtil.getParentOfType(PsiTreeUtil.getParentOfType(element, fit.intellij.json.psi.JsonProperty.class),
                                fit.intellij.json.psi.JsonObject.class, fit.intellij.json.psi.JsonArray.class);
  }

  @NotNull
  @Override
  public Collection<PsiElement> getRoots(@NotNull PsiFile file) {
    return file instanceof fit.intellij.json.psi.JsonFile ? ContainerUtil.createMaybeSingletonList(((fit.intellij.json.psi.JsonFile)file).getTopLevelValue()) : ContainerUtil.emptyList();
  }

  @Nullable
  @Override
  public PsiElement getPropertyNameElement(PsiElement property) {
    return property instanceof fit.intellij.json.psi.JsonProperty ? ((fit.intellij.json.psi.JsonProperty)property).getNameElement() : null;
  }
}
