// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.highlighting;

import com.intellij.codeInsight.daemon.RainbowVisitor;
import com.intellij.codeInsight.daemon.impl.HighlightVisitor;
import fit.intellij.json.pointer.JsonPointerPosition;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import fit.intellij.json.psi.JsonFile;
import fit.jetbrains.jsonSchema.impl.JsonOriginalPsiWalker;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class JsonRainbowVisitor extends RainbowVisitor {
  private static class Holder {
    private static final Map<String, Set<String>> blacklist = createBlacklist();

    private static Map<String, Set<String>> createBlacklist() {
      Map<String, Set<String>> blacklist = new HashMap<>();
      blacklist.put("package.json", ContainerUtil.set("/dependencies",
                                                      "/devDependencies",
                                                      "/peerDependencies",
                                                      "/scripts",
                                                      "/directories",
                                                      "/optionalDependencies"));
      return blacklist;
    }
  }

  @Override
  public boolean suitableForFile(@NotNull PsiFile file) {
    return file instanceof JsonFile;
  }

  @Override
  public void visit(@NotNull PsiElement element) {
    if (element instanceof fit.intellij.json.psi.JsonProperty) {
      PsiFile file = element.getContainingFile();
      String fileName = file.getName();
      if (Holder.blacklist.containsKey(fileName)) {
        JsonPointerPosition position = JsonOriginalPsiWalker.INSTANCE.findPosition(element, false);
        if (position != null && Holder.blacklist.get(fileName).contains(position.toJsonPointer())) return;
      }
      String name = ((fit.intellij.json.psi.JsonProperty)element).getName();
      addInfo(getInfo(file, ((fit.intellij.json.psi.JsonProperty)element).getNameElement(), name, JsonSyntaxHighlighterFactory.JSON_PROPERTY_KEY));
      fit.intellij.json.psi.JsonValue value = ((fit.intellij.json.psi.JsonProperty)element).getValue();
      if (value instanceof fit.intellij.json.psi.JsonObject) {
        addInfo(getInfo(file, value.getFirstChild(), name, JsonSyntaxHighlighterFactory.JSON_BRACES));
        addInfo(getInfo(file, value.getLastChild(), name, JsonSyntaxHighlighterFactory.JSON_BRACES));
      }
      else if (value instanceof fit.intellij.json.psi.JsonArray) {
        addInfo(getInfo(file, value.getFirstChild(), name, JsonSyntaxHighlighterFactory.JSON_BRACKETS));
        addInfo(getInfo(file, value.getLastChild(), name, JsonSyntaxHighlighterFactory.JSON_BRACKETS));
        for (fit.intellij.json.psi.JsonValue jsonValue : ((fit.intellij.json.psi.JsonArray)value).getValueList()) {
          addSimpleValueInfo(name, file, jsonValue);
        }
      }
      else {
        addSimpleValueInfo(name, file, value);
      }
    }
  }

  private void addSimpleValueInfo(String name, PsiFile file, fit.intellij.json.psi.JsonValue value) {
    if (value instanceof fit.intellij.json.psi.JsonStringLiteral) {
      addInfo(getInfo(file, value, name, JsonSyntaxHighlighterFactory.JSON_STRING));
    }
    else if (value instanceof fit.intellij.json.psi.JsonNumberLiteral) {
      addInfo(getInfo(file, value, name, JsonSyntaxHighlighterFactory.JSON_NUMBER));
    }
    else if (value instanceof fit.intellij.json.psi.JsonLiteral) {
      addInfo(getInfo(file, value, name, JsonSyntaxHighlighterFactory.JSON_KEYWORD));
    }
  }

  @NotNull
  @Override
  public HighlightVisitor clone() {
    return new JsonRainbowVisitor();
  }
}
