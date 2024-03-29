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
package fit.jetbrains.jsonSchema.impl.adapters;

import com.intellij.psi.PsiElement;
import fit.intellij.json.psi.JsonNullLiteral;
import fit.jetbrains.jsonSchema.extension.adapters.JsonArrayValueAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonObjectValueAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class JsonJsonGenericValueAdapter implements JsonValueAdapter {
  @NotNull private final fit.intellij.json.psi.JsonValue myValue;

  public JsonJsonGenericValueAdapter(@NotNull fit.intellij.json.psi.JsonValue value) {myValue = value;}

  @Override
  public boolean isObject() {
    return false;
  }

  @Override
  public boolean isArray() {
    return false;
  }

  @Override
  public boolean isStringLiteral() {
    return myValue instanceof fit.intellij.json.psi.JsonStringLiteral;
  }

  @Override
  public boolean isNumberLiteral() {
    return myValue instanceof fit.intellij.json.psi.JsonNumberLiteral;
  }

  @Override
  public boolean isBooleanLiteral() {
    return myValue instanceof fit.intellij.json.psi.JsonBooleanLiteral;
  }

  @Override
  public boolean isNull() {
    return myValue instanceof JsonNullLiteral;
  }

  @NotNull
  @Override
  public PsiElement getDelegate() {
    return myValue;
  }

  @Nullable
  @Override
  public JsonObjectValueAdapter getAsObject() {
    return null;
  }

  @Nullable
  @Override
  public JsonArrayValueAdapter getAsArray() {
    return null;
  }
}
