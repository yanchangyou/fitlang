// Copyright 2000-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class IfThenElse {
  private final fit.jetbrains.jsonSchema.impl.JsonSchemaObject condition;
  private final fit.jetbrains.jsonSchema.impl.JsonSchemaObject trueBranch;
  private final fit.jetbrains.jsonSchema.impl.JsonSchemaObject falseBranch;

  public IfThenElse(fit.jetbrains.jsonSchema.impl.JsonSchemaObject condition, fit.jetbrains.jsonSchema.impl.JsonSchemaObject trueBranch, fit.jetbrains.jsonSchema.impl.JsonSchemaObject falseBranch) {
    this.condition = condition;
    this.trueBranch = trueBranch;
    this.falseBranch = falseBranch;
  }

  @NotNull
  public fit.jetbrains.jsonSchema.impl.JsonSchemaObject getIf() {
    return condition;
  }

  @Nullable
  public fit.jetbrains.jsonSchema.impl.JsonSchemaObject getThen() {
    return trueBranch;
  }

  @Nullable
  public JsonSchemaObject getElse() {
    return falseBranch;
  }
}
