// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.extension;

import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions;
import fit.jetbrains.jsonSchema.impl.JsonSchemaObject;
import fit.jetbrains.jsonSchema.impl.JsonSchemaType;

public interface JsonSchemaValidation {
  void validate(JsonValueAdapter propValue,
                JsonSchemaObject schema,
                JsonSchemaType schemaType,
                JsonValidationHost consumer,
                JsonComplianceCheckerOptions options);
}
