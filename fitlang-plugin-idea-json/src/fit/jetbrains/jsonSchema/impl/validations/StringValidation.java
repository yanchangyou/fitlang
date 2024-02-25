// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl.validations;

import fit.intellij.json.JsonBundle;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import fit.jetbrains.jsonSchema.extension.JsonErrorPriority;
import fit.jetbrains.jsonSchema.extension.JsonSchemaValidation;
import fit.jetbrains.jsonSchema.extension.JsonValidationHost;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions;
import fit.jetbrains.jsonSchema.impl.JsonSchemaAnnotatorChecker;
import fit.jetbrains.jsonSchema.impl.JsonSchemaObject;
import fit.jetbrains.jsonSchema.impl.JsonSchemaType;

public class StringValidation implements JsonSchemaValidation {
  public static final StringValidation INSTANCE = new StringValidation();
  @Override
  public void validate(JsonValueAdapter propValue,
                       fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                       JsonSchemaType schemaType,
                       fit.jetbrains.jsonSchema.extension.JsonValidationHost consumer,
                       JsonComplianceCheckerOptions options) {
    checkString(propValue.getDelegate(), schema, consumer);
  }

  private static void checkString(PsiElement propValue,
                                  JsonSchemaObject schema,
                                  JsonValidationHost consumer) {
    String v = JsonSchemaAnnotatorChecker.getValue(propValue, schema);
    if (v == null) return;
    final String value = StringUtil.unquoteString(v);
    if (schema.getMinLength() != null) {
      if (value.length() < schema.getMinLength()) {
        consumer.error(JsonBundle.message("schema.validation.string.shorter.than", schema.getMinLength()), propValue, fit.jetbrains.jsonSchema.extension.JsonErrorPriority.LOW_PRIORITY);
        return;
      }
    }
    if (schema.getMaxLength() != null) {
      if (value.length() > schema.getMaxLength()) {
        consumer.error(JsonBundle.message("schema.validation.string.longer.than", schema.getMaxLength()), propValue, fit.jetbrains.jsonSchema.extension.JsonErrorPriority.LOW_PRIORITY);
        return;
      }
    }
    if (schema.getPattern() != null) {
      if (schema.getPatternError() != null) {
        consumer.error(JsonBundle.message("schema.validation.invalid.string.pattern", StringUtil.convertLineSeparators(schema.getPatternError())),
              propValue, fit.jetbrains.jsonSchema.extension.JsonErrorPriority.LOW_PRIORITY);
      }
      if (!schema.checkByPattern(value)) {
        consumer.error(JsonBundle.message("schema.validation.string.violates.pattern", StringUtil.convertLineSeparators(schema.getPattern())), propValue, JsonErrorPriority.LOW_PRIORITY);
      }
    }
    // I think we are not gonna to support format, there are a couple of RFCs there to check upon..
    /*
    if (schema.getFormat() != null) {
      LOG.info("Unsupported property used: 'format'");
    }*/
  }
}
