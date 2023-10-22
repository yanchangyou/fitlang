// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license that can be found in the LICENSE file.
package fit.jetbrains.jsonSchema.impl.validations;

import fit.intellij.json.JsonBundle;
import fit.intellij.json.pointer.JsonPointerPosition;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.ThreeState;
import fit.jetbrains.jsonSchema.extension.JsonErrorPriority;
import fit.jetbrains.jsonSchema.extension.JsonSchemaValidation;
import fit.jetbrains.jsonSchema.extension.JsonValidationHost;
import fit.jetbrains.jsonSchema.extension.adapters.JsonObjectValueAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonPropertyAdapter;
import fit.jetbrains.jsonSchema.extension.adapters.JsonValueAdapter;
import fit.jetbrains.jsonSchema.impl.JsonSchemaObject;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class ObjectValidation implements JsonSchemaValidation {
  public static final ObjectValidation INSTANCE = new ObjectValidation();
  @Override
  public void validate(JsonValueAdapter propValue,
                       fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                       fit.jetbrains.jsonSchema.impl.JsonSchemaType schemaType,
                       JsonValidationHost consumer,
                       fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions options) {
    checkObject(propValue, schema, consumer, options);
  }
  private static void checkObject(@NotNull JsonValueAdapter value,
                                  @NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                                  JsonValidationHost consumer, fit.jetbrains.jsonSchema.impl.JsonComplianceCheckerOptions options) {
    final JsonObjectValueAdapter object = value.getAsObject();
    if (object == null) return;

    final List<JsonPropertyAdapter> propertyList = object.getPropertyList();
    final Set<String> set = new HashSet<>();
    for (JsonPropertyAdapter property : propertyList) {
      final String name = StringUtil.notNullize(property.getName());
      fit.jetbrains.jsonSchema.impl.JsonSchemaObject propertyNamesSchema = schema.getPropertyNamesSchema();
      if (propertyNamesSchema != null) {
        JsonValueAdapter nameValueAdapter = property.getNameValueAdapter();
        if (nameValueAdapter != null) {
          JsonValidationHost checker = consumer.checkByMatchResult(nameValueAdapter, consumer.resolve(propertyNamesSchema), options);
          if (checker != null) {
            consumer.addErrorsFrom(checker);
          }
        }
      }

      final JsonPointerPosition step = JsonPointerPosition.createSingleProperty(name);
      final Pair<ThreeState, fit.jetbrains.jsonSchema.impl.JsonSchemaObject> pair = fit.jetbrains.jsonSchema.impl.JsonSchemaVariantsTreeBuilder.doSingleStep(step, schema, false);
      if (ThreeState.NO.equals(pair.getFirst()) && !set.contains(name)) {
        consumer.error(JsonBundle.message("json.schema.annotation.not.allowed.property", name), property.getDelegate(),
              fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.ProhibitedProperty,
              new fit.jetbrains.jsonSchema.impl.JsonValidationError.ProhibitedPropertyIssueData(name), JsonErrorPriority.LOW_PRIORITY);
      }
      else if (ThreeState.UNSURE.equals(pair.getFirst())) {
        for (JsonValueAdapter propertyValue : property.getValues()) {
          consumer.checkObjectBySchemaRecordErrors(pair.getSecond(), propertyValue);
        }
      }
      set.add(name);
    }

    if (object.shouldCheckIntegralRequirements() || options.isForceStrict()) {
      final Set<String> required = schema.getRequired();
      if (required != null) {
        HashSet<String> requiredNames = new LinkedHashSet<>(required);
        requiredNames.removeAll(set);
        if (!requiredNames.isEmpty()) {
          fit.jetbrains.jsonSchema.impl.JsonValidationError.MissingMultiplePropsIssueData data = createMissingPropertiesData(schema, requiredNames, consumer);
          consumer.error(JsonBundle.message("schema.validation.missing.required.property.or.properties",  data.getMessage(false)), value.getDelegate(), fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.MissingProperty, data,
                JsonErrorPriority.MISSING_PROPS);
        }
      }
      if (schema.getMinProperties() != null && propertyList.size() < schema.getMinProperties()) {
        consumer.error(JsonBundle.message("schema.validation.number.of.props.less.than", schema.getMinProperties()), value.getDelegate(), JsonErrorPriority.LOW_PRIORITY);
      }
      if (schema.getMaxProperties() != null && propertyList.size() > schema.getMaxProperties()) {
        consumer.error(JsonBundle.message("schema.validation.number.of.props.greater.than", schema.getMaxProperties()), value.getDelegate(), JsonErrorPriority.LOW_PRIORITY);
      }
      final Map<String, List<String>> dependencies = schema.getPropertyDependencies();
      if (dependencies != null) {
        for (Map.Entry<String, List<String>> entry : dependencies.entrySet()) {
          if (set.contains(entry.getKey())) {
            final List<String> list = entry.getValue();
            HashSet<String> deps = new HashSet<>(list);
            deps.removeAll(set);
            if (!deps.isEmpty()) {
              fit.jetbrains.jsonSchema.impl.JsonValidationError.MissingMultiplePropsIssueData data = createMissingPropertiesData(schema, deps, consumer);
              consumer.error(
                JsonBundle.message("schema.validation.violated.dependency", data.getMessage(false), entry.getKey()),
                    value.getDelegate(),
                    fit.jetbrains.jsonSchema.impl.JsonValidationError.FixableIssueKind.MissingProperty,
                    data, JsonErrorPriority.MISSING_PROPS);
            }
          }
        }
      }
      final Map<String, fit.jetbrains.jsonSchema.impl.JsonSchemaObject> schemaDependencies = schema.getSchemaDependencies();
      if (schemaDependencies != null) {
        for (Map.Entry<String, fit.jetbrains.jsonSchema.impl.JsonSchemaObject> entry : schemaDependencies.entrySet()) {
          if (set.contains(entry.getKey())) {
            consumer.checkObjectBySchemaRecordErrors(entry.getValue(), value);
          }
        }
      }
    }
  }

  private static fit.jetbrains.jsonSchema.impl.JsonValidationError.MissingMultiplePropsIssueData createMissingPropertiesData(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema,
                                                                                                                             HashSet<String> requiredNames,
                                                                                                                             JsonValidationHost consumer) {
    List<fit.jetbrains.jsonSchema.impl.JsonValidationError.MissingPropertyIssueData> allProps = new ArrayList<>();
    for (String req: requiredNames) {
      fit.jetbrains.jsonSchema.impl.JsonSchemaObject propertySchema = resolvePropertySchema(schema, req);
      Object defaultValue = propertySchema == null ? null : propertySchema.getDefault();
      Ref<Integer> enumCount = Ref.create(0);

      fit.jetbrains.jsonSchema.impl.JsonSchemaType type = null;

      if (propertySchema != null) {
        fit.jetbrains.jsonSchema.impl.MatchResult result = null;
        Object valueFromEnum = getDefaultValueFromEnum(propertySchema, enumCount);
        if (valueFromEnum != null) {
          defaultValue = valueFromEnum;
        }
        else {
          result = consumer.resolve(propertySchema);
          if (result.mySchemas.size() == 1) {
            valueFromEnum = getDefaultValueFromEnum(result.mySchemas.get(0), enumCount);
            if (valueFromEnum != null) {
              defaultValue = valueFromEnum;
            }
          }
        }
        type = propertySchema.getType();
        if (type == null) {
          if (result == null) {
            result = consumer.resolve(propertySchema);
          }
          if (result.mySchemas.size() == 1) {
            type = result.mySchemas.get(0).getType();
          }
        }
      }
      allProps.add(new fit.jetbrains.jsonSchema.impl.JsonValidationError.MissingPropertyIssueData(req,
                                                                    type,
                                                                    defaultValue,
                                                                    enumCount.get()));
    }

    return new fit.jetbrains.jsonSchema.impl.JsonValidationError.MissingMultiplePropsIssueData(allProps);
  }

  private static fit.jetbrains.jsonSchema.impl.JsonSchemaObject resolvePropertySchema(@NotNull fit.jetbrains.jsonSchema.impl.JsonSchemaObject schema, String req) {
    if (schema.getProperties().containsKey(req)) {
      return schema.getProperties().get(req);
    }
    else {
      fit.jetbrains.jsonSchema.impl.JsonSchemaObject propertySchema = schema.getMatchingPatternPropertySchema(req);
      if (propertySchema != null) {
        return propertySchema;
      }
      else {
        fit.jetbrains.jsonSchema.impl.JsonSchemaObject additionalPropertiesSchema = schema.getAdditionalPropertiesSchema();
        if (additionalPropertiesSchema != null) {
          return additionalPropertiesSchema;
        }
      }
    }
    return null;
  }


  @Nullable
  private static Object getDefaultValueFromEnum(@NotNull JsonSchemaObject propertySchema, @NotNull Ref<Integer> enumCount) {
    List<Object> enumValues = propertySchema.getEnum();
    if (enumValues != null) {
      enumCount.set(enumValues.size());
      if (enumValues.size() == 1) {
        Object defaultObject = enumValues.get(0);
        return defaultObject instanceof String ? StringUtil.unquoteString((String)defaultObject) : defaultObject;
      }
    }
    return null;
  }
}
