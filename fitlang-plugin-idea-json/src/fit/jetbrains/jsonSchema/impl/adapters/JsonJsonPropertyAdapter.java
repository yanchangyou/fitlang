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

import fit.intellij.json.psi.JsonArray;
import fit.intellij.json.psi.JsonObject;
import fit.intellij.json.psi.JsonProperty;
import fit.intellij.json.psi.JsonValue;
import com.intellij.psi.PsiElement;
import com.intellij.util.containers.ContainerUtil;
import fit.jetbrains.jsonSchema.extension.adapters.JsonObjectValueAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonPropertyAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.Collections;

/**
 * @author Irina.Chernushina on 2/20/2017.
 */
public class JsonJsonPropertyAdapter implements JsonPropertyAdapter {
  @NotNull private final JsonProperty myProperty;

  public JsonJsonPropertyAdapter(@NotNull JsonProperty property) {
    myProperty = property;
  }

  @Nullable
  @Override
  public String getName() {
    return myProperty.getName();
  }

  @NotNull
  @Override
  public Collection<fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter> getValues() {
    return myProperty.getValue() == null ? ContainerUtil.emptyList() : Collections.singletonList(createAdapterByType(myProperty.getValue()));
  }

  @Nullable
  @Override
  public fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter getNameValueAdapter() {
    return createAdapterByType(myProperty.getNameElement());
  }

  @NotNull
  @Override
  public PsiElement getDelegate() {
    return myProperty;
  }

  @Nullable
  @Override
  public JsonObjectValueAdapter getParentObject() {
    return myProperty.getParent() instanceof JsonObject ? new JsonJsonObjectAdapter((JsonObject)myProperty.getParent()) : null;
  }

  @NotNull
  public static JsonValueAdapter createAdapterByType(@NotNull JsonValue value) {
    if (value instanceof JsonObject) return new JsonJsonObjectAdapter((JsonObject)value);
    if (value instanceof JsonArray) return new JsonJsonArrayAdapter((JsonArray)value);
    return new JsonJsonGenericValueAdapter(value);
  }
}
