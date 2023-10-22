// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl.validations;

import fit.jetbrains.jsonSchema.extension.JsonSchemaValidation;
import fit.jetbrains.jsonSchema.extension.JsonValidationHost;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions;
import fit.jetbrains.jsonSchema.impl.JsonSchemaAnnotatorChecker;
import fit.jetbrains.jsonSchema.impl.JsonSchemaObject;
import fit.jetbrains.jsonSchema.impl.JsonSchemaType;

import java.util.Collections;

public class TypeValidation implements JsonSchemaValidation {
  public static final TypeValidation INSTANCE = new TypeValidation();
  @Override
  public void validate(JsonValueAdapter propValue,
                       JsonSchemaObject schema,
                       JsonSchemaType schemaType,
                       JsonValidationHost consumer,
                       JsonComplianceCheckerOptions options) {
    JsonSchemaType otherType = JsonSchemaAnnotatorChecker.getMatchingSchemaType(schema, schemaType);
    if (otherType != null && !otherType.equals(schemaType) && !otherType.equals(propValue.getAlternateType(schemaType))) {
      consumer.typeError(propValue.getDelegate(), propValue.getAlternateType(schemaType), JsonSchemaAnnotatorChecker.getExpectedTypes(Collections.singleton(schema)));
    }
  }
}
