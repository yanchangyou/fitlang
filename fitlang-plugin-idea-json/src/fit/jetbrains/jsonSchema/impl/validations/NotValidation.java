// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl.validations;

import fit.intellij.json.JsonBundle;
import fit.jetbrains.jsonSchema.extension.JsonErrorPriority;
import fit.jetbrains.jsonSchema.extension.JsonSchemaValidation;
import fit.jetbrains.jsonSchema.extension.JsonValidationHost;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions;
import fit.jetbrains.jsonSchema.impl.JsonSchemaObject;
import fit.jetbrains.jsonSchema.impl.JsonSchemaType;
import fit.jetbrains.jsonSchema.impl.MatchResult;

import java.util.Collection;

public class NotValidation implements JsonSchemaValidation {
  public static final NotValidation INSTANCE = new NotValidation();
  @Override
  public void validate(JsonValueAdapter propValue,
                       JsonSchemaObject schema,
                       JsonSchemaType schemaType,
                       JsonValidationHost consumer,
                       JsonComplianceCheckerOptions options) {
    final MatchResult result = consumer.resolve(schema.getNot());
    if (result.mySchemas.isEmpty() && result.myExcludingSchemas.isEmpty()) return;

    // if 'not' uses reference to owning schema back -> do not check, seems it does not make any sense
    if (result.mySchemas.stream().anyMatch(s -> schema.equals(s)) ||
        result.myExcludingSchemas.stream().flatMap(Collection::stream)
          .anyMatch(s -> schema.equals(s))) return;

    final JsonValidationHost checker = consumer.checkByMatchResult(propValue, result, options.withForcedStrict());
    if (checker == null || checker.isValid()) consumer.error(JsonBundle.message("schema.validation.against.not"), propValue.getDelegate(), JsonErrorPriority.NOT_SCHEMA);
  }
}
