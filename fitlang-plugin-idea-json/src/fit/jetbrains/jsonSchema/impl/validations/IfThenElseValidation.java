// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl.validations;

import fit.jetbrains.jsonSchema.extension.JsonSchemaValidation;
import fit.jetbrains.jsonSchema.extension.JsonValidationHost;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.impl.IfThenElse;

import java.util.List;

public class IfThenElseValidation implements JsonSchemaValidation {
  public static final IfThenElseValidation INSTANCE = new IfThenElseValidation();
  @Override
  public void validate(JsonValueAdapter propValue,
                       fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                       fit.jetbrains.jsonSchema.impl.JsonSchemaType schemaType,
                       JsonValidationHost consumer,
                       fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions options) {
    List<fit.jetbrains.jsonSchema.impl.IfThenElse> ifThenElseList = schema.getIfThenElse();
    assert ifThenElseList != null;
    for (IfThenElse ifThenElse : ifThenElseList) {
      fit.jetbrains.jsonSchema.impl.MatchResult result = consumer.resolve(ifThenElse.getIf());
      if (result.mySchemas.isEmpty() && result.myExcludingSchemas.isEmpty()) return;

      final JsonValidationHost checker = consumer.checkByMatchResult(propValue, result, options.withForcedStrict());
      if (checker != null) {
        if (checker.isValid()) {
          fit.jetbrains.jsonSchema.impl.JsonSchemaObject then = ifThenElse.getThen();
          if (then != null) {
            consumer.checkObjectBySchemaRecordErrors(then, propValue);
          }
        }
        else {
          fit.jetbrains.jsonSchema.impl.JsonSchemaObject schemaElse = ifThenElse.getElse();
          if (schemaElse != null) {
            consumer.checkObjectBySchemaRecordErrors(schemaElse, propValue);
          }
        }
      }
    }
  }
}
