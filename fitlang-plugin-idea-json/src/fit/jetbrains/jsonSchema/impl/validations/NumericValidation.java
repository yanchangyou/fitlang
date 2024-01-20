// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl.validations;

import fit.intellij.json.JsonBundle;
import com.intellij.psi.PsiElement;
import fit.jetbrains.jsonSchema.extension.JsonErrorPriority;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.extension.JsonSchemaValidation;
import fit.jetbrains.jsonSchema.extension.JsonValidationHost;
import fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions;

public class NumericValidation implements JsonSchemaValidation {
  public static final NumericValidation INSTANCE = new NumericValidation();
  private static void checkNumber(PsiElement propValue,
                                  fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                                  fit.jetbrains.jsonSchema.impl.JsonSchemaType schemaType,
                                  fit.jetbrains.jsonSchema.extension.JsonValidationHost consumer) {
    Number value;
    String valueText = fit.jetbrains.jsonSchema.impl.JsonSchemaAnnotatorChecker.getValue(propValue, schema);
    if (valueText == null) return;
    if (fit.jetbrains.jsonSchema.impl.JsonSchemaType._integer.equals(schemaType)) {
      value = fit.jetbrains.jsonSchema.impl.JsonSchemaType.getIntegerValue(valueText);
      if (value == null) {
        consumer.error(JsonBundle.message("schema.validation.integer.expected"), propValue,
                       fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.TypeMismatch,
                       new fit.jetbrains.jsonSchema.impl.JsonValidationError.TypeMismatchIssueData(new fit.jetbrains.jsonSchema.impl.JsonSchemaType[]{schemaType}), JsonErrorPriority.TYPE_MISMATCH);
        return;
      }
    }
    else {
      try {
        value = Double.valueOf(valueText);
      }
      catch (NumberFormatException e) {
        if (!fit.jetbrains.jsonSchema.impl.JsonSchemaType._string_number.equals(schemaType)) {
          consumer.error(JsonBundle.message("schema.validation.number.expected"), propValue,
                fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.TypeMismatch,
                new fit.jetbrains.jsonSchema.impl.JsonValidationError.TypeMismatchIssueData(new fit.jetbrains.jsonSchema.impl.JsonSchemaType[]{schemaType}), JsonErrorPriority.TYPE_MISMATCH);
        }
        return;
      }
    }
    final Number multipleOf = schema.getMultipleOf();
    if (multipleOf != null) {
      final double leftOver = value.doubleValue() % multipleOf.doubleValue();
      if (leftOver > 0.000001) {
        final String multipleOfValue = String.valueOf(Math.abs(multipleOf.doubleValue() - multipleOf.intValue()) < 0.000001 ?
                                                      multipleOf.intValue() : multipleOf);
        consumer.error(JsonBundle.message("schema.validation.not.multiple.of", multipleOfValue), propValue, JsonErrorPriority.LOW_PRIORITY);
        return;
      }
    }

    checkMinimum(schema, value, propValue, consumer);
    checkMaximum(schema, value, propValue, consumer);
  }

  private static void checkMaximum(fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                                   Number value,
                                   PsiElement propertyValue,
                                   fit.jetbrains.jsonSchema.extension.JsonValidationHost consumer) {

    Number exclusiveMaximumNumber = schema.getExclusiveMaximumNumber();
    if (exclusiveMaximumNumber != null) {
      final double doubleValue = exclusiveMaximumNumber.doubleValue();
      if (value.doubleValue() >= doubleValue) {
        consumer.error(JsonBundle.message("schema.validation.greater.than.exclusive.maximum", exclusiveMaximumNumber), propertyValue, JsonErrorPriority.LOW_PRIORITY);
      }
    }
    Number maximum = schema.getMaximum();
    if (maximum == null) return;
    boolean isExclusive = Boolean.TRUE.equals(schema.isExclusiveMaximum());
    final double doubleValue = maximum.doubleValue();
    if (isExclusive) {
      if (value.doubleValue() >= doubleValue) {
        consumer.error(JsonBundle.message("schema.validation.greater.than.exclusive.maximum", maximum), propertyValue, JsonErrorPriority.LOW_PRIORITY);
      }
    }
    else {
      if (value.doubleValue() > doubleValue) {
        consumer.error(JsonBundle.message("schema.validation.greater.than.maximum", maximum), propertyValue, JsonErrorPriority.LOW_PRIORITY);
      }
    }
  }

  private static void checkMinimum(fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                                   Number value,
                                   PsiElement propertyValue,
                                   fit.jetbrains.jsonSchema.extension.JsonValidationHost consumer) {
    // schema v6 - exclusiveMinimum is numeric now
    Number exclusiveMinimumNumber = schema.getExclusiveMinimumNumber();
    if (exclusiveMinimumNumber != null) {
      final double doubleValue = exclusiveMinimumNumber.doubleValue();
      if (value.doubleValue() <= doubleValue) {
        consumer.error(JsonBundle.message("schema.validation.less.than.exclusive.minimum", exclusiveMinimumNumber), propertyValue, JsonErrorPriority.LOW_PRIORITY);
      }
    }

    Number minimum = schema.getMinimum();
    if (minimum == null) return;
    boolean isExclusive = Boolean.TRUE.equals(schema.isExclusiveMinimum());
    final double doubleValue = minimum.doubleValue();
    if (isExclusive) {
      if (value.doubleValue() <= doubleValue) {
        consumer.error(JsonBundle.message("schema.validation.less.than.exclusive.minimum", minimum), propertyValue, JsonErrorPriority.LOW_PRIORITY);
      }
    }
    else {
      if (value.doubleValue() < doubleValue) {
        consumer.error(JsonBundle.message("schema.validation.less.than.minimum", minimum), propertyValue, JsonErrorPriority.LOW_PRIORITY);
      }
    }
  }

  @Override
  public void validate(JsonValueAdapter propValue,
                       fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                       fit.jetbrains.jsonSchema.impl.JsonSchemaType schemaType,
                       JsonValidationHost consumer,
                       JsonComplianceCheckerOptions options) {
    checkNumber(propValue.getDelegate(), schema, schemaType, consumer);
  }
}
