// Copyright 2000-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.intellij.json.pointer;

import fit.intellij.json.psi.JsonArray;
import fit.intellij.json.psi.JsonObject;
import fit.intellij.json.psi.JsonProperty;
import fit.intellij.json.psi.JsonValue;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class JsonPointerResolver {
  private final fit.intellij.json.psi.JsonValue myRoot;
  private final String myPointer;

  public JsonPointerResolver(@NotNull fit.intellij.json.psi.JsonValue root, @NotNull String pointer) {
    myRoot = root;
    myPointer = pointer;
  }

  @Nullable
  public fit.intellij.json.psi.JsonValue resolve() {
    fit.intellij.json.psi.JsonValue root = myRoot;
    final List<fit.intellij.json.pointer.JsonPointerPosition.Step> steps = fit.intellij.json.pointer.JsonPointerPosition.parsePointer(myPointer).getSteps();
    for (JsonPointerPosition.Step step : steps) {
      String name = step.getName();
      if (name != null) {
        if (!(root instanceof fit.intellij.json.psi.JsonObject)) return null;
        fit.intellij.json.psi.JsonProperty property = ((fit.intellij.json.psi.JsonObject)root).findProperty(name);
        root = property == null ? null : property.getValue();
      }
      else {
        int idx = step.getIdx();
        if (idx < 0) return null;

        if (!(root instanceof fit.intellij.json.psi.JsonArray)) {
          if (root instanceof fit.intellij.json.psi.JsonObject) {
            JsonProperty property = ((JsonObject)root).findProperty(String.valueOf(idx));
            if (property == null) {
              return null;
            }
            root = property.getValue();
            continue;
          }
          else {
            return null;
          }
        }
        List<JsonValue> list = ((JsonArray)root).getValueList();
        if (idx >= list.size()) return null;
        root = list.get(idx);
      }
    }
    return root;
  }
}
