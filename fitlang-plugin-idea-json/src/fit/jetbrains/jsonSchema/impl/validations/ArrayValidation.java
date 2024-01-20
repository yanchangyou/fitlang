// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl.validations;

import fit.intellij.json.JsonBundle;
import com.intellij.util.containers.MultiMap;
import fit.jetbrains.jsonSchema.extension.JsonErrorPriority;
import fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker;
import fit.jetbrains.jsonSchema.extension.JsonSchemaValidation;
import fit.jetbrains.jsonSchema.extension.JsonValidationHost;
import fit.jetbrains.jsonSchema.extension.adapters.JsonArrayValueAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions;
import fit.jetbrains.jsonSchema.impl.JsonSchemaObject;
import fit.jetbrains.jsonSchema.impl.JsonSchemaType;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

public class ArrayValidation implements JsonSchemaValidation {
  public static final ArrayValidation INSTANCE = new ArrayValidation();
  @Override
  public void validate(fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter propValue,
                       fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                       JsonSchemaType schemaType,
                       fit.jetbrains.jsonSchema.extension.JsonValidationHost consumer,
                       fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions options) {
    checkArray(propValue, schema, consumer, options);
  }

  private static void checkArray(fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter value,
                                 fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                                 fit.jetbrains.jsonSchema.extension.JsonValidationHost consumer,
                                 fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions options) {
    final JsonArrayValueAdapter asArray = value.getAsArray();
    if (asArray == null) return;
    final List<fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter> elements = asArray.getElements();
    checkArrayItems(value, elements, schema, consumer, options);
  }

  private static void checkArrayItems(@NotNull fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter array,
                                      @NotNull final List<fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter> list,
                                      final fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                                      fit.jetbrains.jsonSchema.extension.JsonValidationHost consumer,
                                      JsonComplianceCheckerOptions options) {
    if (schema.isUniqueItems()) {
      final MultiMap<String, fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter> valueTexts = new MultiMap<>();
      final fit.jetbrains.jsonSchema.extension.JsonLikePsiWalker walker = JsonLikePsiWalker.getWalker(array.getDelegate(), schema);
      assert walker != null;
      for (fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter adapter : list) {
        valueTexts.putValue(walker.getNodeTextForValidation(adapter.getDelegate()), adapter);
      }

      for (Map.Entry<String, Collection<fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter>> entry: valueTexts.entrySet()) {
        if (entry.getValue().size() > 1) {
          for (fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter item: entry.getValue()) {
            if (!item.shouldCheckAsValue()) continue;
            consumer.error(JsonBundle.message("schema.validation.not.unique"), item.getDelegate(), fit.jetbrains.jsonSchema.extension.JsonErrorPriority.TYPE_MISMATCH);
          }
        }
      }
    }
    if (schema.getContainsSchema() != null) {
      boolean match = false;
      for (fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter item: list) {
        final JsonValidationHost checker = consumer.checkByMatchResult(item, consumer.resolve(schema.getContainsSchema()), options);
        if (checker == null || checker.isValid()) {
          match = true;
          break;
        }
      }
      if (!match) {
        consumer.error(JsonBundle.message("schema.validation.array.not.contains"), array.getDelegate(), fit.jetbrains.jsonSchema.extension.JsonErrorPriority.MEDIUM_PRIORITY);
      }
    }
    if (schema.getItemsSchema() != null) {
      for (fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter item : list) {
        consumer.checkObjectBySchemaRecordErrors(schema.getItemsSchema(), item);
      }
    }
    else if (schema.getItemsSchemaList() != null) {
      final Iterator<JsonSchemaObject> iterator = schema.getItemsSchemaList().iterator();
      for (JsonValueAdapter arrayValue : list) {
        if (iterator.hasNext()) {
          consumer.checkObjectBySchemaRecordErrors(iterator.next(), arrayValue);
        }
        else {
          if (!Boolean.TRUE.equals(schema.getAdditionalItemsAllowed())) {
            consumer.error(JsonBundle.message("schema.validation.array.no.extra"), arrayValue.getDelegate(), fit.jetbrains.jsonSchema.extension.JsonErrorPriority.LOW_PRIORITY);
          }
          else if (schema.getAdditionalItemsSchema() != null) {
            consumer.checkObjectBySchemaRecordErrors(schema.getAdditionalItemsSchema(), arrayValue);
          }
        }
      }
    }
    if (schema.getMinItems() != null && list.size() < schema.getMinItems()) {
      consumer.error(JsonBundle.message("schema.validation.array.shorter.than", schema.getMinItems()), array.getDelegate(), fit.jetbrains.jsonSchema.extension.JsonErrorPriority.LOW_PRIORITY);
    }
    if (schema.getMaxItems() != null && list.size() > schema.getMaxItems()) {
      consumer.error(JsonBundle.message("schema.validation.array.longer.than",  schema.getMaxItems()), array.getDelegate(), fit.jetbrains.jsonSchema.extension.JsonErrorPriority.LOW_PRIORITY);
    }

    // these two are not correct by the schema spec, but are used in some schemas
    if (schema.getMinLength() != null && list.size() < schema.getMinLength()) {
      consumer.error(JsonBundle.message("schema.validation.array.shorter.than", schema.getMinLength()), array.getDelegate(), fit.jetbrains.jsonSchema.extension.JsonErrorPriority.LOW_PRIORITY);
    }
    if (schema.getMaxLength() != null && list.size() > schema.getMaxLength()) {
      consumer.error(JsonBundle.message("schema.validation.array.longer.than",  schema.getMaxLength()), array.getDelegate(), JsonErrorPriority.LOW_PRIORITY);
    }
  }
}
