// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.extension;

public enum JsonErrorPriority {
  NOT_SCHEMA,
  TYPE_MISMATCH,
  MEDIUM_PRIORITY,
  MISSING_PROPS,
  LOW_PRIORITY
}
